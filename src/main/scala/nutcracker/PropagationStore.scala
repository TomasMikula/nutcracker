package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.util.{FreeK, FreeKT, Index, K3Map, KMap, KMapB, Lst, StateInterpreter, Step, Uncons, WriterState, `Forall{* -> *}`}

import scalaz.Id._
import scalaz.{Monad, StateT, |>=|, ~>}
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}

sealed abstract class PropagationStore[Ref[_], K] {
  def fetch[D](ref: Ref[D]): D
  def fetchResult[D](ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out]

  protected def addVariable[D](d: D, dom: Dom[D]): (PropagationStore[Ref, K], Ref[D])
  protected def addDomainObserver[D, U, Δ](ref: Ref[D], f: D => (Option[K], Option[(D, Δ) => Trigger[K]]))(implicit dom: Dom.Aux[D, U, Δ]): (PropagationStore[Ref, K], Lst[K])
  protected def addSelTrigger[L <: HList](sel: Sel[Ref, L], t: L => Trigger[K]): (PropagationStore[Ref, K], Option[K])
  protected def update[D, U, Δ](ref: Ref[D], u: U)(implicit dom: Dom.Aux[D, U, Δ]): PropagationStore[Ref, K]
  protected def uncons: Option[(PropagationStore[Ref, K], Lst[K])]
}

object PropagationStore {

  def empty[K]: PropagationStore[DRef, K] = PropagationStoreImpl[K](
    nextId = 0L,
    domains = KMap[DRef, λ[D => (D, Dom[D])]](),
    domainObservers = K3Map[DRef.Aux, λ[(D, U, Δ) => List[(D, Δ) => Trigger[K]]]](),
    selTriggers = KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => Trigger[K]]], HList](),
    cellsToSels = Index.empty(sel => sel.cells),
    unresolvedVars = Set(),
    failedVars = Set(),
    dirtyDomains = K3Map[DRef.Aux, λ[(D, U, Δ) => Δ]](),
    dirtySelections = Set()
  )

  def emptyF[F[_[_], _]]: PropagationStore[DRef, FreeK[F, Unit]] =
    empty

  def naiveAssess[K[_]](implicit ord: K |>=| FreeK[PropagationLang, ?]): PropagationStore[DRef, K[Unit]] => Assessment[List[K[Unit]]] =
    s => s match {
      case ps @ PropagationStoreImpl(_, _, _, _, _, _, _, _, _) => ps.naiveAssess(ord[Unit](_))
    }

  def naiveAssess[K[_], S[_]](
    lens: Lens[S[K[Unit]], PropagationStore[DRef, K[Unit]]])(implicit
    ord: K |>=| FreeK[PropagationLang, ?]
  ): S[K[Unit]] => Assessment[List[K[Unit]]] =
    s => (naiveAssess[K](ord))(lens.get(s))


  private def fetch: λ[A => DRef[Promise[A]]] ~> (PropagationStore[DRef, FreeK[PropagationLang, Unit]] => ?) =
    λ[λ[A => DRef[Promise[A]]] ~> (PropagationStore[DRef, FreeK[PropagationLang, Unit]] => ?)](
      pa => s => s.fetchResult(pa).get
    )

  val interpreter: StateInterpreter[PropagationLang, PropagationStore[DRef, ?]] =
    new StateInterpreter[PropagationLang, PropagationStore[DRef, ?]] {

      def step: Step[PropagationLang, PropagationStore[DRef, ?]] =
        new Step[PropagationLang, PropagationStore[DRef, ?]] {
          import PropagationLang._
          override def apply[K[_], A](p: PropagationLang[K, A]): WriterState[Lst[K[Unit]], PropagationStore[DRef, K[Unit]], A] = WriterState(s =>
            p match {
              case Cell(d, dom) => s.addVariable(d, dom) match {
                case (s1, ref) => (Lst.empty, s1, ref)
              }
              case Observe(ref, f, dom) => s.addDomainObserver(ref, f)(dom) match {
                case (s1, ks) => (ks, s1, ())
              }
              case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
                case (s1, ok) => (Lst.maybe(ok), s1, ())
              }
              case Update(ref, u, dom) => (Lst.empty, s.update(ref, u)(dom), ())
            }
          )
        }

      def uncons: Uncons[PropagationStore[DRef, ?]] = Uncons[PropagationStore[DRef, ?]](
        new `Forall{* -> *}`[λ[K => StateT[Option, PropagationStore[DRef, K], Lst[K]]]] {
          override def compute[K]: StateT[Option, PropagationStore[DRef, K], Lst[K]] =
            StateT(_.uncons)
        })
    }

  def dfsSolver: DFSSolver[PropagationLang, PropagationStore[DRef, ?], Id, λ[A => DRef[Promise[A]]]] = {
    implicit val mfp: Monad[FreeKT[PropagationLang, Id, ?]] = FreeKT.freeKTMonad[PropagationLang, Id]
    new DFSSolver[PropagationLang, PropagationStore[DRef, ?], Id, λ[A => DRef[Promise[A]]]](
      interpreter.freeInstance,
      emptyF[PropagationLang],
      naiveAssess[FreeK[PropagationLang, ?]],
      fetch)
  }

}

private[nutcracker] case class PropagationStoreImpl[K] private(
  nextId: Long,
  domains: KMap[DRef, λ[D => (D, Dom[D])]],
  domainObservers: K3Map[DRef.Aux, λ[(D, U, Δ) => List[(D, Δ) => Trigger[K]]]],
  selTriggers: KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => Trigger[K]]], HList],
  cellsToSels: Index[DRef[_], Sel[DRef, _ <: HList]],
  unresolvedVars: Set[DRef[D] forSome { type D }],
  failedVars: Set[Long],
  dirtyDomains: K3Map[DRef.Aux, λ[(D, U, Δ) => Δ]],
  dirtySelections: Set[Sel[DRef, _ <: HList]]
) extends PropagationStore[DRef, K] {
  import shapeless.PolyDefns.~>

  private val cellFetcher: DRef ~> shapeless.Id = new (DRef ~> shapeless.Id) {
    def apply[D](cell: DRef[D]): D = fetch(cell)
  }

  def addVariable[D](d: D, dom: Dom[D]): (PropagationStoreImpl[K], DRef[D]) = {
    val ref = DRef[D](nextId)(dom)
    val domains1 = domains.put(ref)((d, dom: Dom.Aux[D, dom.Update, dom.Delta]))
    val (unresolvedVars1, failedVars1) = dom.assess(d) match {
      case Dom.Failed => (unresolvedVars, failedVars + nextId)
      case Dom.Refined => (unresolvedVars, failedVars)
      case Dom.Unrefined(_) => (unresolvedVars + ref, failedVars)
    }
    (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: DRef[D]): D = domains(ref)._1

  def fetchResult[D](ref: DRef[D])(implicit fin: Final[D]): Option[fin.Out] = fin.extract(fetch(ref))

  def fetchVector[D, N <: Nat](refs: Sized[Vector[DRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  protected def update[D, U, Δ](ref: DRef[D], u: U)(implicit dom: Dom.Aux[D, U, Δ]): PropagationStoreImpl[K] = {
    val d0 = domains(ref)._1
    dom.update(d0, u) match {
      case None => this
      case Some((d1, diff1)) =>
        val (unresolvedVars1, failedVars1) = dom.assess(d1) match {
          case Dom.Failed => (unresolvedVars - ref, failedVars + ref.domainId)
          case Dom.Refined => (unresolvedVars - ref, failedVars)
          case Dom.Unrefined(_) => (unresolvedVars, failedVars)
        }
        val domains1 = domains.put(ref)((d1, dom))
        val dirtyDomains1 = dirtyDomains.updated(ref.infer)(diff1)(dom.combineDeltas)
        copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }
  }

  def addDomainObserver[D, U, Δ](ref: DRef[D], f: D => (Option[K], Option[(D, Δ) => Trigger[K]]))(implicit dom: Dom.Aux[D, U, Δ]): (PropagationStoreImpl[K], Lst[K]) = {
    val (now, onChange) = f(fetch(ref))
    onChange match {
      case Some(action) =>
        val (s1, ks) = addDomainObserver0(ref.infer, action)
        (s1, now ?+: ks)
      case None => (this, Lst.maybe(now))
    }
  }

  def addSelTrigger[L <: HList](sel: Sel[DRef, L], t: L => Trigger[K]): (PropagationStoreImpl[K], Option[K]) = {
    t(sel.fetch(cellFetcher)) match {
      case Discard() => (this, None)
      case Sleep() => (addSelTrigger0(sel, t), None)
      case Fire(cont) => (this, Some(cont))
      case FireReload(cont) => (addSelTrigger0(sel, t), Some(cont))
    }
  }

  private def addDomainObserver0[D, U, Δ](ref: DRef.Aux[D, U, Δ], f: (D, Δ) => Trigger[K]): (PropagationStoreImpl[K], Lst[K]) = {
    val triggers = domainObservers.getOrElse(ref)(Nil)
    val (remainingTriggers, firedTriggers) = dirtyDomains.get(ref) match {
      case Some(δ) => collectDomObservers(fetch(ref), δ, triggers)
      case None => (triggers, Lst.empty)
    }
    (
      copy(
        domainObservers = domainObservers.put(ref)(f :: remainingTriggers),
        dirtyDomains = dirtyDomains - ref
      ),
      firedTriggers
    )
  }

  private def addSelTrigger0[L <: HList](sel: Sel[DRef, L], t: L => Trigger[K]): PropagationStoreImpl[K] = {
    copy(
      selTriggers = selTriggers.put(sel)(t :: selTriggers.getOrElse(sel)(Nil)),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  private def triggersForDomain[D, U, Δ](ref: DRef.Aux[D, U, Δ], δ: Δ): (PropagationStoreImpl[K], Lst[K]) =
    collectDomObservers(fetch(ref), δ, domainObservers.getOrElse(ref)(Nil)) match {
      case (Nil, fired) => (copy(domainObservers = domainObservers - ref), fired)
      case (forLater, fired) => (copy(domainObservers = domainObservers.put(ref)(forLater)), fired)
    }

  private def triggersForSel[L <: HList](sel: Sel[DRef, L]): (PropagationStoreImpl[K], Lst[K]) = {
    val d = sel.fetch(cellFetcher)
    collectSelTriggers(d, selTriggers.getOrElse(sel)(Nil)) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers.put(sel)(forLater)), fired)
    }
  }

  private def getSelsForCell(ref: DRef[_]): Set[Sel[DRef, _ <: HList]] = cellsToSels.get(ref)

  private def collectDomObservers[D, Δ](d: D, δ: Δ, triggers: List[(D, Δ) => Trigger[K]]): (List[(D, Δ) => Trigger[K]], Lst[K]) =
    triggers match {
      case Nil => (Nil, Lst.empty)
      case t :: ts =>
        val (ts1, conts) = collectDomObservers(d, δ, ts)
        t(d, δ) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }

  private def collectSelTriggers[L <: HList](l: L, triggers: List[L => Trigger[K]]): (List[L => Trigger[K]], Lst[K]) =
    triggers match {
      case Nil => (Nil, Lst.empty)
      case t :: ts =>
        val (ts1, conts) = collectSelTriggers(l, ts)
        t(l) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }

  protected def uncons: Option[(PropagationStoreImpl[K], Lst[K])] =
    if(dirtyDomains.nonEmpty) {
      val h = dirtyDomains.head
      val dirtySels = dirtySelections union getSelsForCell(h._1)
      val (s1, ks) = triggersForDomain(h._1, h._2)
      Some((s1.copy(dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
    } else if(dirtySelections.nonEmpty) {
      val sel = dirtySelections.head
      val (s1, ks) = triggersForSel(sel)
      Some((s1.copy(dirtySelections = dirtySelections.tail), ks))
    }
    else None

  private[nutcracker] def naiveAssess(inj: FreeK[PropagationLang, Unit] => K): Assessment[List[K]] =
    if(failedVars.nonEmpty) Failed
    else if(unresolvedVars.isEmpty) Done
    else {
      def splitDomain[D](ref: DRef[D]): Option[List[K]] = {
        val (d, domain) = domains(ref)
        domain.assess(d) match {
          case Dom.Unrefined(choices) => choices() map { _ map { ui => inj(Propagation[FreeK[PropagationLang, ?], DRef].update(ref)(domain).by(ui)) } }
          case _ => sys.error("splitDomain should be called on unresolved variables only.")
        }
      }

      unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }
}

private[nutcracker] object PropagationStoreImpl {
}
