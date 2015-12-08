package nutcracker

import scala.language.{existentials, higherKinds}

import nutcracker.util.Index
import nutcracker.util.free.{MonoidK, Interpreter}

import shapeless.{HList, Nat, Sized}
import shapeless.PolyDefns.~>

import scalaz._
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.monoid._

import Domain._

case class PropagationStore[K[_]] private(
  nextId: Long,
  domains: Map[Long, (D, Domain[A, D]) forSome { type A; type D }],
  domainTriggers: Map[CellRef[_], List[_ => Trigger[K]]],
  selTriggers: Map[Sel[_], List[_ => Trigger[K]]],
  cellsToSels: Index[Sel[_ <: HList], CellRef[_]],
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
    val d1 = dom.meet(d0, d)
    if(dom.eqv(d0, d1))
      None
    else {
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

  private def addDomainTrigger0[D](ref: CellRef[D], t: D => Trigger[K]): PropagationStore[K] =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

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


  def addSelTrigger[L <: HList](sel: Sel[L], t: L => Trigger[K]): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )
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
    domains: Set[CellRef[D] forSome { type D }],
    selections: Set[Sel[_ <: HList]]
  ) {
    def uncons: Option[(DirtyThing, DirtyThings[K])] =
      if(domains.nonEmpty) Some((DirtyDomain(domains.head), this.copy(domains = domains.tail)))
      else if(selections.nonEmpty) Some((DirtySel(selections.head), this.copy(selections = selections.tail)))
      else None
  }

  object DirtyThings {
    def empty[K[_]]: DirtyThings[K] = DirtyThings(Set(), Set())

    implicit def monoidK: MonoidK[DirtyThings] = new MonoidK[DirtyThings] {
      def zero[K[_]]: DirtyThings[K] = DirtyThings.empty
      def append[K[_]](x: DirtyThings[K], y: DirtyThings[K]): DirtyThings[K] = DirtyThings(
        x.domains ++ y.domains,
        x.selections ++ y.selections)
    }
    implicit def monoid[K[_]]: Monoid[DirtyThings[K]] = monoidK.monoid

    def dirtyDomain[K[_], D](ref: CellRef[D]): DirtyThings[K] = DirtyThings(Set(ref), Set())
    def dirtyDomains[K[_], D](refs: Iterable[CellRef[D]]): DirtyThings[K] = DirtyThings(refs.toSet, Set())
    def dirtySel[K[_]](sel: Sel[_ <: HList]): DirtyThings[K] = DirtyThings(Set(), Set(sel))
    def dirtySels[K[_]](sels: Set[Sel[_ <: HList]]): DirtyThings[K] = DirtyThings(Set(), sels)
  }

  sealed trait DirtyThing
  case class DirtyDomain[D](ref: CellRef[D]) extends DirtyThing
  case class DirtySel(sel: Sel[_ <: HList]) extends DirtyThing

  import DirtyThings._

  implicit def interpreter: Interpreter[PropagationLang, PropagationStore, DirtyThings] =
    new Interpreter[PropagationLang, PropagationStore, DirtyThings] {

      def step[K[_]: Applicative, A](p: PropagationLang[K, A])(s: PropagationStore[K]): (PropagationStore[K], DirtyThings[K], K[A]) = {
        p match {
          case Variable(d, dom) => s.addVariable(d, dom) match {
            case (s1, ref) => (s1, DirtyThings.empty, ref.point[K])
          }
          case VarTrigger(ref, f) => s.addDomainTrigger(ref, f) match {
            case (s1, ok) => (s1, DirtyThings.empty, ok.getOrElse(().point[K]))
          }
          case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
            case s1 => (s1, dirtySel(sel), ().point[K])
          }
          case Intersect(ref, d) => s.intersect(ref, d) match {
            case Some(s1) => (s1, dirtyDomain(ref), ().point[K])
            case None => (s, DirtyThings.empty[K], ().point[K])
          }
          case IntersectVector(refs, values) => s.intersectVector(refs, values) match {
            case (s1, dirtyCells) => (s1, dirtyDomains(dirtyCells), ().point[K])
          }
          case Fetch(ref) => (s, DirtyThings.empty[K], s.fetch(ref).point[K])
          case FetchVector(refs) => (s, DirtyThings.empty[K], s.fetchVector(refs).point[K])
          case WhenResolved(ref, f) => s.addDomainResolutionTrigger(ref, f) match {
            case (s1, ok) => (s1, DirtyThings.empty[K], ok.getOrElse(().point[K]))
          }
        }
      }

      def uncons[K[_]: Applicative](w: DirtyThings[K])(s: PropagationStore[K]): Option[(K[Unit], DirtyThings[K], PropagationStore[K])] = w.uncons match {
        case None => None
        case Some((dt, dts)) => dt match {
          case DirtyDomain(ref) => s.triggersForDomain(ref) match {
            case (s1, ks) => Some((Foldable[List].sequence_(ks), dts |+| dirtySels[K](s.getSelsForCell(ref)), s1))
          }
          case DirtySel(sel) => s.triggersForSel(sel) match {
            case (s1, ks) => Some((Foldable[List].sequence_(ks), dts, s1))
          }
        }
      }
    }
}