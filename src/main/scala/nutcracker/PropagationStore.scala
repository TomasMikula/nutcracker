package nutcracker

import scala.language.{existentials, higherKinds}
import monocle.Lens
import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.util.{FreeK, Index, KMap, StateInterpreterT, Uncons, ValK, ~~>}
import nutcracker.util.StepT.Step

import scalaz.Id._
import scalaz.StateT
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}
import PropagationStore._

case class PropagationStore[K[_]] private(
  nextId: Long,
  domains: Domains,
  domainTriggers: Map[VRef[_], List[(_, _) => Trigger[K]]],
  selTriggers: Map[Sel[_], List[_ => Trigger[K]]],
  cellsToSels: Index[VRef[_], Sel[_ <: HList]],
  unresolvedVars: Set[DRef[D, U, Δ] forSome { type D; type U; type Δ }],
  failedVars: Set[Long],
  dirtyDomains: KMap[DRef[Any, Nothing, ?], Id],
  dirtySelections: Set[Sel[_ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: VRef ~> shapeless.Id = new ~>[VRef, shapeless.Id] {
    def apply[D](cell: VRef[D]): D = fetch(cell)
  }

  def addVariable[D, U, Δ](d: D, ev: Dom[D, U, Δ]): (PropagationStore[K], DRef[D, U, Δ]) = {
    val domains1 = domains + ((nextId, (d, ev)))
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

  def fetchResult[A, D](ref: DRef[D, _, _])(implicit ee: EmbedExtract[A, D]): Option[A] = getDomain(ref) match {
    case (d, _) => ee.extract(d)
  }

  def fetchVector[D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  private def update[D, U, Δ](ref: DRef[D, U, Δ], u: U): PropagationStore[K] = {
    val (d0, dom) = getDomain(ref)
    dom.update(d0, u) match {
      case None => this
      case Some((d1, diff1)) =>
        val (unresolvedVars1, failedVars1) = dom.assess(d1) match {
          case Dom.Failed => (unresolvedVars - ref, failedVars + ref.domainId)
          case Dom.Refined => (unresolvedVars - ref, failedVars)
          case Dom.Unrefined(_) => (unresolvedVars, failedVars)
        }
        val domains1 = domains + ((ref.domainId, (d1, dom)))
        val dirtyDomains1 = dirtyDomains.updated(ref, diff1, dom.combineDiffs)
        copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }
  }

  def addDomainTrigger[D, U, Δ](ref: DRef[D, U, Δ], t: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K]])): (PropagationStore[K], Option[K[Unit]]) = {
    val (now, onChange) = t(fetch(ref))
    onChange match {
      case Some(action) => (addDomainTrigger0(ref, action), now)
      case None => (this, now)
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

  private def addDomainTrigger0[D, U, Δ](ref: DRef[D, U, Δ], t: (D, Δ) => Trigger[K]): PropagationStore[K] =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

  private def addSelTrigger0[L <: HList](sel: Sel[L], t: L => Trigger[K]): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  private def triggersForDomain[D, U, Δ](ref: DRef[D, U, Δ], δ: Δ): (PropagationStore[K], List[K[Unit]]) =
    collectDomTriggers(fetch(ref), δ, domainTriggers.getOrElse(ref, Nil).asInstanceOf[List[(D, Δ) => Trigger[K]]]) match {
      case (Nil, fired) => (copy(domainTriggers = domainTriggers - ref), fired)
      case (forLater, fired) => (copy(domainTriggers = domainTriggers + ((ref, forLater))), fired)
    }

  private def triggersForSel[L <: HList](sel: Sel[L]): (PropagationStore[K], List[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectSelTriggers(d, selTriggers.getOrElse(sel, Nil).asInstanceOf[List[L => Trigger[K]]]) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers + ((sel, forLater))), fired)
    }
  }

  private def getSelsForCell(ref: DRef[_, _, _]): Set[Sel[_ <: HList]] = cellsToSels.get(ref)


  private[nutcracker] def getDomain[D, U, Δ](ref: DRef[D, U, Δ]): (D, Dom[D, U, Δ]) =
    domains(ref.domainId).asInstanceOf[(D, Dom[D, U, Δ])]

  private def collectDomTriggers[D, Δ](d: D, δ: Δ, triggers: List[(D, Δ) => Trigger[K]]): (List[(D, Δ) => Trigger[K]], List[K[Unit]]) =
    triggers match {
      case Nil => (Nil, Nil)
      case t :: ts =>
        val (ts1, conts) = collectDomTriggers(d, δ, ts)
        t(d, δ) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }

  private def collectSelTriggers[L <: HList](l: L, triggers: List[L => Trigger[K]]): (List[L => Trigger[K]], List[K[Unit]]) =
    triggers match {
      case Nil => (Nil, Nil)
      case t :: ts =>
        val (ts1, conts) = collectSelTriggers(l, ts)
        t(l) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }

  private def uncons: Option[(PropagationStore[K], List[K[Unit]])] =
    if(dirtyDomains.nonEmpty) {
      val (d, δ) = dirtyDomains.head
      val dirtySels = dirtySelections union getSelsForCell(d)
      val (s1, ks) = triggersForDomain(d, δ)
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

  type Domains = Map[Long, (D, Dom[D, U, Δ]) forSome { type D; type U; type Δ }]

  def empty[K[_]] = PropagationStore[K](
    nextId = 0L,
    domains = Map(),
    domainTriggers = Map(),
    selTriggers = Map(),
    cellsToSels = Index.empty(sel => sel.cells),
    unresolvedVars = Set(),
    failedVars = Set(),
    dirtyDomains = KMap[DRef[Any, Nothing, ?], Id](),
    dirtySelections = Set()
  )

  val interpreter: StateInterpreterT.StateInterpreter.Aux[PropagationLang, PropagationStore] =
    new StateInterpreterT.StateInterpreter[PropagationLang] {
      type State[K[_]] = PropagationStore[K]

      def step: Step[PropagationLang, State] =
        Step(new (PropagationLang ~~> λ[(K[_], A) => scalaz.State[State[K], (A, List[K[Unit]])]]) {
          override def apply[K[_], A](p: PropagationLang[K, A]): scalaz.State[PropagationStore[K], (A, List[K[Unit]])] = scalaz.State(s =>
            p match {
              case Cell(d, dom) => s.addVariable(d, dom) match {
                case (s1, ref) => (s1, (ref, Nil))
              }
              case DomTrigger(ref, f) => s.addDomainTrigger(ref, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
              case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
              case Update(ref, u) => (s.update(ref, u), ((), Nil))
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
        val (d, domain) = s.getDomain(ref)
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