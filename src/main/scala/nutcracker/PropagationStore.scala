package nutcracker

import scala.language.{existentials, higherKinds}
import monocle.Lens
import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.util.{FreeK, Index, StateInterpreterT, Uncons, ValK, ~~>}
import nutcracker.util.StepT.Step

import scalaz.Id._
import scalaz.StateT
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}
import Domain._

case class PropagationStore[K[_]] private(
  nextId: Long,
  domains: Map[Long, (D, Dom[D, U, Δ], Option[Δ]) forSome { type D; type U; type Δ }],
  domainTriggers: Map[VRef[_], List[_ => Trigger[K]]],
  selTriggers: Map[Sel[_], List[_ => Trigger[K]]],
  cellsToSels: Index[VRef[_], Sel[_ <: HList]],
  unresolvedVars: Set[DRef[D, U, Δ] forSome { type D; type U; type Δ }],
  failedVars: Set[Long],
  dirtyDomains: Set[DRef[D, U, Δ] forSome { type D; type U; type Δ }],
  dirtySelections: Set[Sel[_ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: VRef ~> shapeless.Id = new ~>[VRef, shapeless.Id] {
    def apply[D](cell: VRef[D]): D = fetch(cell)
  }

  def addVariable[D, U, Δ](d: D, ev: Dom[D, U, Δ]): (PropagationStore[K], DRef[D, U, Δ]) = {
    val domains1 = domains + ((nextId, (d, ev, Option.empty)))
    val ref = DRef[D, U, Δ](nextId)
    val (unresolvedVars1, failedVars1) = ev.assess(d) match {
      case Dom.Failed => (unresolvedVars, failedVars + nextId)
      case Dom.Refined => (unresolvedVars, failedVars)
      case Dom.Unrefined(_) => (unresolvedVars + ref, failedVars)
    }
    (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: VRef[D]): D = ref match {
    case dr @ DRef(_) =>  getDomain(dr)._1
  }

  def fetchResult[A, D](ref: DRef[D, _, _])(implicit dom: Domain[A, D]): Option[A] = getDomain(ref) match {
    case (d, _, _) => dom.values(d) match {
      case Just(a) => Some(a)
      case _ => None
    }
  }

  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def intersect[D](ref: LRef[D], d: D): PropagationStore[K] = update[D, D, D](ref, d)

  def intersectVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]): PropagationStore[K] =
    (refs zip values).foldLeft[PropagationStore[K]](this)((s, rv) => s.intersect(rv._1, rv._2))

  private def update[D, U, Δ](ref: DRef[D, U, Δ], u: U): PropagationStore[K] = {
    val (d0, dom, diff0) = getDomain(ref)
    dom.update(d0, u) match {
      case None => this
      case Some((d1, diff1)) =>
        val (unresolvedVars1, failedVars1) = dom.assess(d1) match {
          case Dom.Failed => (unresolvedVars - ref, failedVars + ref.domainId)
          case Dom.Refined => (unresolvedVars - ref, failedVars)
          case Dom.Unrefined(_) => (unresolvedVars, failedVars)
        }
        val diff = Option(diff0.fold(diff1)(dom.combineDiffs(_, diff1)))
        val domains1 = domains + ((ref.domainId, (d1, dom, diff)))
        val dirtyDomains1 = dirtyDomains + ref
        copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }
  }

  def addDomainTrigger[D](ref: VRef[D], t: D => Trigger[K]): (PropagationStore[K], Option[K[Unit]]) = {
    t(fetch(ref)) match {
      case Discard() => (this, None)
      case Sleep() => (addDomainTrigger0(ref, t), None)
      case Fire(cont) => (this, Some(cont))
      case FireReload(cont) => (addDomainTrigger0(ref, t), Some(cont))
    }
  }

  def addSelTrigger[L <: HList](sel: Sel[L], t: L => Trigger[K]): (PropagationStore[K], Option[K[Unit]]) = {
    t(sel.fetch(cellFetcher)) match {
      case Discard() => (this, None)
      case Sleep() => (addSelTrigger0(sel, t), None)
      case Fire(cont) => (this, Some(cont))
      case FireReload(cont) => (addSelTrigger0(sel, t), Some(cont))
    }
  }

  private def addDomainTrigger0[D](ref: VRef[D], t: D => Trigger[K]): PropagationStore[K] =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

  private def addSelTrigger0[L <: HList](sel: Sel[L], t: L => Trigger[K]): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  def triggersForDomain[D](ref: DRef[D, _, _]): (PropagationStore[K], List[K[Unit]]) = {
    val d = fetch(ref)
    collectTriggers(d, domainTriggers.getOrElse(ref, Nil).asInstanceOf[List[D => Trigger[K]]]) match {
      case (Nil, fired) => (copy(domainTriggers = domainTriggers - ref), fired)
      case (forLater, fired) => (copy(domainTriggers = domainTriggers + ((ref, forLater))), fired)
    }
  }

  def triggersForSel[L <: HList](sel: Sel[L]): (PropagationStore[K], List[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectTriggers(d, selTriggers.getOrElse(sel, Nil).asInstanceOf[List[L => Trigger[K]]]) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers + ((sel, forLater))), fired)
    }
  }

  def getSelsForCell(ref: DRef[_, _, _]): Set[Sel[_ <: HList]] = cellsToSels.get(ref)


  private[nutcracker] def getDomain[D, U, Δ](ref: DRef[D, U, Δ]): (D, Dom[D, U, Δ], Option[Δ]) =
    domains(ref.domainId).asInstanceOf[(D, Dom[D, U, Δ], Option[Δ])]

//  private def setDomain[A, D](ref: DomRef[A, D], d: D, dom: Domain[A, D]): PropagationStore[K] =
//    copy(domains = domains + ((ref.domainId, (d, dom))))

  private def collectTriggers[D](d: D, triggers: List[D => Trigger[K]]): (List[D => Trigger[K]], List[K[Unit]]) =
    triggers match {
      case Nil => (Nil, Nil)
      case t :: ts =>
        val (ts1, conts) = collectTriggers(d, ts)
        t(d) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }

  private def uncons: Option[(PropagationStore[K], List[K[Unit]])] =
    if(dirtyDomains.nonEmpty) {
      val d = dirtyDomains.head
      val dirtySels = dirtySelections union getSelsForCell(d)
      val (s1, ks) = triggersForDomain(d)
      Some((s1.copy(dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
    } else if(dirtySelections.nonEmpty) {
      val sel = dirtySelections.head
      val (s1, ks) = triggersForSel(sel)
      Some((s1.copy(dirtySelections = dirtySelections.tail), ks))
    }
    else None
}

object PropagationStore {
  import PropagationLang._
  import scalaz.~>

  def empty[K[_]] = PropagationStore[K](
    nextId = 0L,
    domains = Map(),
    domainTriggers = Map(),
    selTriggers = Map(),
    cellsToSels = Index.empty(sel => sel.cells),
    unresolvedVars = Set(),
    failedVars = Set(),
    dirtyDomains = Set(),
    dirtySelections = Set()
  )

  val interpreter: StateInterpreterT.StateInterpreter.Aux[PropagationLang, PropagationStore] =
    new StateInterpreterT.StateInterpreter[PropagationLang] {
      type State[K[_]] = PropagationStore[K]

      def step: Step[PropagationLang, State] =
        Step(new (PropagationLang ~~> λ[(K[_], A) => scalaz.State[State[K], (A, List[K[Unit]])]]) {
          override def apply[K[_], A](p: PropagationLang[K, A]): scalaz.State[PropagationStore[K], (A, List[K[Unit]])] = scalaz.State(s =>
            p match {
              case Variable(d, dom) => s.addVariable(d, dom) match {
                case (s1, ref) => (s1, (ref, Nil))
              }
              case VarTrigger(ref, f) => s.addDomainTrigger(ref, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
              case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
              case Update(ref, u) => (s.update(ref, u), ((), Nil))
              case Intersect(ref, d) => (s.intersect(ref, d), ((), Nil))
              case IntersectVector(refs, values) => (s.intersectVector(refs, values), ((), Nil))
              case Fetch(ref) => (s, (s.fetch(ref), Nil))
              case FetchVector(refs) => (s, (s.fetchVector(refs), Nil))
            }
          )
        })

      def uncons: Uncons[PropagationStore] = Uncons[PropagationStore](
        new ValK[λ[K[_] => StateT[Option, PropagationStore[K], List[K[Unit]]]]] {
          override def compute[K[_]]: StateT[Option, PropagationStore[K], List[K[Unit]]] =
            StateT(_.uncons)
        })
    }

  def naiveAssess[K[_]](implicit tr: FreeK[PropagationLang, ?] ~> K): PropagationStore[K] => Assessment[List[K[Unit]]] = s => {
    if(s.failedVars.nonEmpty) Failed
    else if(s.unresolvedVars.isEmpty) Done
    else {
      def splitDomain[D, U](ref: DRef[D, U, _]): Option[List[K[Unit]]] = {
        val (d, domain, _) = s.getDomain(ref)
        domain.assess(d) match {
          case Dom.Unrefined(choices) => choices() map { _ map { ui => tr(updateF(ref)(ui)) } }
          case _ => sys.error("splitDomain should be called on unresolved variables only.")
        }
      }

      s.unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }
  }

  def naiveAssess[K[_], S[_[_]]](
    lens: Lens[S[K], PropagationStore[K]])(implicit
    tr: FreeK[PropagationLang, ?] ~> K
  ): S[K] => Assessment[List[K[Unit]]] =
    s => (naiveAssess[K](tr))(lens.get(s))


  private def fetch: Promised ~> (PropagationStore[FreeK[PropagationLang, ?]] => ?) = new ~>[Promised, PropagationStore[FreeK[PropagationLang, ?]] => ?] {
    def apply[A](pa: Promised[A]): (PropagationStore[FreeK[PropagationLang, ?]] => A) = s => s.fetchResult(pa).get
  }
  def dfsSolver: DFSSolver[PropagationLang, PropagationStore, Id, Promised] =
    new DFSSolver[PropagationLang, PropagationStore, Id, Promised](
      interpreter.freeInstance,
      empty[FreeK[PropagationLang, ?]],
      naiveAssess[FreeK[PropagationLang, ?]],
      fetch)
}