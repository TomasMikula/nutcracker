package nutcracker

import scala.language.{existentials, higherKinds}

import monocle.Lens
import nutcracker.Assessment.{Stuck, Incomplete, Done, Failed}
import nutcracker.util.Index
import nutcracker.util.free.{FreeK, MonoidK, Interpreter}
import scalaz.{Applicative, Foldable, Monoid}
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.monoid._
import shapeless.{HList, Nat, Sized}
import shapeless.PolyDefns.~>

import Domain._

case class PropagationStore[K[_]] private(
  nextId: Long,
  domains: Map[Long, (D, Domain[A, D]) forSome { type A; type D }],
  domainTriggers: Map[CellRef[_], List[_ => Trigger[K]]],
  selTriggers: Map[Sel[_], List[_ => Trigger[K]]],
  cellsToSels: Index[CellRef[_], Sel[_ <: HList]],
  unresolvedVars: Set[DomRef[A, D] forSome { type A; type D }],
  failedVars: Set[Long]) {

  private val cellFetcher: CellRef ~> shapeless.Id = new ~>[CellRef, shapeless.Id] {
    def apply[D](cell: CellRef[D]): D = fetch(cell)
  }

  def addVariable[A, D](d: D, ev: Domain[A, D]): (PropagationStore[K], DomRef[A, D]) = {
    val domains1 = domains + ((nextId, (d, ev)))
    val ref = DomRef[A, D](nextId)
    val (unresolvedVars1, failedVars1) = ev.values(d) match {
      case Empty() => (unresolvedVars, failedVars + nextId)
      case Just(a) => (unresolvedVars, failedVars)
      case Many(_) => (unresolvedVars + ref, failedVars)
    }
    (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: CellRef[D]): D = ref match {
    case dr@DomRef(_) => getDomain(dr)._1
  }

  def fetchResult[A, D](ref: DomRef[A, D]): Option[A] = getDomain(ref) match {
    case (d, dom) => dom.values(d) match {
      case Just(a) => Some(a)
      case _ => None
    }
  }

  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def intersect[D](ref: CellRef[D], d: D): Option[PropagationStore[K]] = ref match {
    case dr @ DomRef(_) => intersect(dr, d)
  }

  def intersectVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]): (PropagationStore[K], List[CellRef[D]]) =
    (refs zip values).foldLeft[(PropagationStore[K], List[CellRef[D]])]((this, Nil)) {
      case ((doms, dirtyCells), (ref, d)) => intersect(ref, d) match {
        case Some(doms1) => (doms1, ref :: dirtyCells)
        case None => (doms, dirtyCells)
      }
    }

  private def intersect[A, D](ref: DomRef[A, D], d: D): Option[PropagationStore[K]] = {
    val (d0, dom) = getDomain(ref)
    dom.refine(d0, d) match {
      case None => None
      case Some(d1) =>
        val (unresolvedVars1, failedVars1) = dom.values(d1) match {
          case Empty() => (unresolvedVars - ref, failedVars + ref.domainId)
          case Just(_) => (unresolvedVars - ref, failedVars)
          case Many(_) => (unresolvedVars, failedVars)
        }
        val domains1 = domains + ((ref.domainId, (d1, dom)))
        Some(copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1))
    }
  }

  def addDomainTrigger[D](ref: CellRef[D], t: D => Trigger[K]): (PropagationStore[K], Option[K[Unit]]) = {
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

  private def addDomainTrigger0[D](ref: CellRef[D], t: D => Trigger[K]): PropagationStore[K] =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

  private def addSelTrigger0[L <: HList](sel: Sel[L], t: L => Trigger[K]): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  def triggersForDomain[D](ref: CellRef[D]): (PropagationStore[K], List[K[Unit]]) = {
    val d = fetch(ref)
    collectTriggers(d, domainTriggers.getOrElse(ref, Nil).asInstanceOf[List[D => Trigger[K]]]) match {
      case (Nil, fired) => (copy(domainTriggers = domainTriggers - ref), fired)
      case (forLater, fired) => (copy(domainTriggers = domainTriggers + ((ref, forLater))), fired)
    }
  }

  def addDomainResolutionTrigger[A, D](ref: DomRef[A, D], f: A => K[Unit]): (PropagationStore[K], Option[K[Unit]]) = {
    val domain = getDomain(ref)._2
    addDomainTrigger(ref, (d: D) => domain.values(d) match {
      case Domain.Empty() => Discard()
      case Domain.Just(a) => Fire(f(a))
      case Domain.Many(_) => Sleep()
    })
  }

  def triggersForSel[L <: HList](sel: Sel[L]): (PropagationStore[K], List[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectTriggers(d, selTriggers.getOrElse(sel, Nil).asInstanceOf[List[L => Trigger[K]]]) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers + ((sel, forLater))), fired)
    }
  }

  def getSelsForCell(ref: CellRef[_]): Set[Sel[_ <: HList]] = cellsToSels.get(ref)


  private[nutcracker] def getDomain[A, D](ref: DomRef[A, D]): (D, Domain[A, D]) =
    domains(ref.domainId).asInstanceOf[(D, Domain[A, D])]

  private def setDomain[A, D](ref: DomRef[A, D], d: D, dom: Domain[A, D]): PropagationStore[K] =
    copy(domains = domains + ((ref.domainId, (d, dom))))

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
}

object PropagationStore {
  import PropagationLang._

  def empty[K[_]] = PropagationStore[K](
    nextId = 0L,
    domains = Map(),
    domainTriggers = Map(),
    selTriggers = Map(),
    cellsToSels = Index.empty(sel => sel.cells),
    unresolvedVars = Set(),
    failedVars = Set()
  )

  case class DirtyThings[K[_]](
    continuations: List[K[Unit]],
    domains: Set[CellRef[D] forSome { type D }],
    selections: Set[Sel[_ <: HList]]
  ) {
    def uncons: Option[(DirtyThing[K], DirtyThings[K])] =
      if(continuations.nonEmpty) Some((Continuation(continuations.head), this.copy(continuations = continuations.tail)))
      else if(domains.nonEmpty) Some((DirtyDomain(domains.head), this.copy(domains = domains.tail)))
      else if(selections.nonEmpty) Some((DirtySel(selections.head), this.copy(selections = selections.tail)))
      else None
  }

  object DirtyThings {
    def empty[K[_]]: DirtyThings[K] = DirtyThings(Nil, Set(), Set())

    implicit val monoidK: MonoidK[DirtyThings] = new MonoidK[DirtyThings] {
      def zero[K[_]]: DirtyThings[K] = DirtyThings.empty
      def append[K[_]](x: DirtyThings[K], y: DirtyThings[K]): DirtyThings[K] = DirtyThings(
        if(x.continuations.size < y.continuations.size) x.continuations ++ y.continuations else y.continuations ++ x.continuations,
        x.domains ++ y.domains,
        x.selections ++ y.selections)
    }
    implicit def monoid[K[_]]: Monoid[DirtyThings[K]] = monoidK.monoid

    def continuation[K[_]](k: K[Unit]): DirtyThings[K] = DirtyThings(k::Nil, Set(), Set())
    def dirtyDomain[K[_], D](ref: CellRef[D]): DirtyThings[K] = DirtyThings(Nil, Set(ref), Set())
    def dirtyDomains[K[_], D](refs: Iterable[CellRef[D]]): DirtyThings[K] = DirtyThings(Nil, refs.toSet, Set())
    def dirtySel[K[_]](sel: Sel[_ <: HList]): DirtyThings[K] = DirtyThings(Nil, Set(), Set(sel))
    def dirtySels[K[_]](sels: Set[Sel[_ <: HList]]): DirtyThings[K] = DirtyThings(Nil, Set(), sels)
  }

  sealed trait DirtyThing[K[_]]
  case class Continuation[K[_]](k: K[Unit]) extends DirtyThing[K]
  case class DirtyDomain[K[_], D](ref: CellRef[D]) extends DirtyThing[K]
  case class DirtySel[K[_]](sel: Sel[_ <: HList]) extends DirtyThing[K]

  import DirtyThings._

  implicit def interpreter: Interpreter.Aux[PropagationLang, PropagationStore, DirtyThings] =
    new Interpreter[PropagationLang] {
      type State[K[_]] = PropagationStore[K]
      type Dirty[K[_]] = DirtyThings[K]

      def step[K[_]: Applicative, A](p: PropagationLang[K, A])(s: PropagationStore[K]): (PropagationStore[K], DirtyThings[K], A) = {
        p match {
          case Variable(d, dom) => s.addVariable(d, dom) match {
            case (s1, ref) => (s1, DirtyThings.empty, ref)
          }
          case VarTrigger(ref, f) => s.addDomainTrigger(ref, f) match {
            case (s1, ok) => (s1, ok.map(continuation(_)).getOrElse(DirtyThings.empty), ())
          }
          case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
            case (s1, ok) => (s1, ok.map(continuation(_)).getOrElse(DirtyThings.empty), ())
          }
          case Intersect(ref, d) => s.intersect(ref, d) match {
            case Some(s1) => (s1, dirtyDomain(ref), ())
            case None => (s, DirtyThings.empty[K], ())
          }
          case IntersectVector(refs, values) => s.intersectVector(refs, values) match {
            case (s1, dirtyCells) => (s1, dirtyDomains(dirtyCells), ())
          }
          case Fetch(ref) => (s, DirtyThings.empty[K], s.fetch(ref))
          case FetchVector(refs) => (s, DirtyThings.empty[K], s.fetchVector(refs))
          case WhenResolved(ref, f) => s.addDomainResolutionTrigger(ref, f) match {
            case (s1, ok) => (s1, ok.map(continuation(_)).getOrElse(DirtyThings.empty[K]), ())
          }
        }
      }

      def uncons[K[_]: Applicative](w: DirtyThings[K])(s: PropagationStore[K]): Option[(K[Unit], DirtyThings[K], PropagationStore[K])] = w.uncons match {
        case None => None
        case Some((dt, dts)) => dt match {
          case Continuation(k) => Some((k, dts, s))
          case DirtyDomain(ref) => s.triggersForDomain(ref) match {
            case (s1, ks) => Some((Foldable[List].sequence_(ks), dts |+| dirtySels[K](s.getSelsForCell(ref)), s1))
          }
          case DirtySel(sel) => s.triggersForSel(sel) match {
            case (s1, ks) => Some((Foldable[List].sequence_(ks), dts, s1))
          }
        }
      }

      def emptyState[K[_]]: PropagationStore[K] = PropagationStore.empty
      def dirtyMonoidK: MonoidK[Dirty] = DirtyThings.monoidK
    }

  import scalaz.~>
  def naiveAssess[K[_]: Applicative](implicit tr: FreeK[PropagationLang, ?] ~> K): PropagationStore[K] => Assessment[List[K[Unit]]] = s => {
    if(s.failedVars.nonEmpty) Failed
    else if(s.unresolvedVars.isEmpty) Done
    else {
      def splitDomain[A, D](ref: DomRef[A, D]): Option[List[K[Unit]]] = {
        val (d, domain) = s.getDomain(ref)
        domain.values(d) match {
          case Domain.Empty() => Some(Nil)
          case Domain.Just(a) => Some(().pure[K] :: Nil)
          case Domain.Many(branchings) =>
            if(branchings.isEmpty) None
            else Some(branchings.head map { d => tr(intersectF(ref)(d)) })
        }
      }

      s.unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }
  }

  def naiveAssess[K[_]: Applicative, S[_[_]]](
    lens: Lens[S[K], PropagationStore[K]])(implicit
    tr: FreeK[PropagationLang, ?] ~> K
  ): S[K] => Assessment[List[K[Unit]]] =
    s => (naiveAssess[K](Applicative[K], tr))(lens.get(s))


  private def fetch: Promised ~> (PropagationStore[FreeK[PropagationLang, ?]] => ?) = new ~>[Promised, PropagationStore[FreeK[PropagationLang, ?]] => ?] {
    def apply[A](pa: Promised[A]): (PropagationStore[FreeK[PropagationLang, ?]] => A) = s => s.fetchResult(pa).get
  }
  def dfsSolver: DFSSolver[PropagationLang, PropagationStore, Promised] =
    new DFSSolver[PropagationLang, PropagationStore, Promised](interpreter, naiveAssess[FreeK[PropagationLang, ?]], fetch)
}