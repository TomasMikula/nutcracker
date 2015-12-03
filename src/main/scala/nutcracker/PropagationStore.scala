package nutcracker

import scala.language.{existentials, higherKinds}

import nutcracker.util.free.Interpreter
import shapeless.HList

import scalaz._
import scalaz.std.list._
import scalaz.syntax.applicative._

object PropagationStore {
  import PropagationLang._

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

    implicit def monoid[K[_]]: Monoid[DirtyThings[K]] = new Monoid[DirtyThings[K]] {
      def zero: DirtyThings[K] = DirtyThings.empty
      def append(x: DirtyThings[K], y: => DirtyThings[K]): DirtyThings[K] = DirtyThings(
        x.domains ++ y.domains,
        x.selections ++ y.selections)
    }

    def dirtyDomain[K[_], D](ref: CellRef[D]): DirtyThings[K] = DirtyThings(Set(ref), Set())
    def dirtyDomains[K[_], D](refs: Iterable[CellRef[D]]): DirtyThings[K] = DirtyThings(refs.toSet, Set())
    def dirtySel[K[_]](sel: Sel[_ <: HList]): DirtyThings[K] = DirtyThings(Set(), Set(sel))
    def dirtySels[K[_]](sels: Set[Sel[_ <: HList]]): DirtyThings[K] = DirtyThings(Set(), sels)
  }

  sealed trait DirtyThing
  case class DirtyDomain[D](ref: CellRef[D]) extends DirtyThing
  case class DirtySel(sel: Sel[_ <: HList]) extends DirtyThing


  def interpreter: Interpreter[PropagationLang, Domains, DirtyThings] =
    new Interpreter[PropagationLang, Domains, DirtyThings] {

      def step[K[_]: Applicative, A](p: PropagationLang[K, A])(s: Domains[K]): (Domains[K], DirtyThings[K], K[A]) = {
        p match {
          case Variable(d, dom) => s.addVariable(d, dom) match {
            case (s1, ref) => (s1, DirtyThings.empty, ref.point[K])
          }
          case VarTrigger(ref, f) => s.addDomainTrigger(ref, f) match {
            case (s1, ok) => (s1, DirtyThings.empty, ok.getOrElse(().point[K]))
          }
          case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
            case s1 => (s1, DirtyThings.dirtySel(sel), ().point[K])
          }
          case Intersect(ref, d) => s.intersect(ref, d) match {
            case Some(s1) => (s1, DirtyThings.dirtyDomain(ref), ().point[K])
            case None => (s, DirtyThings.empty[K], ().point[K])
          }
          case IntersectVector(refs, values) => s.intersectVector(refs, values) match {
            case (s1, dirtyCells) => (s1, DirtyThings.dirtyDomains(dirtyCells), ().point[K])
          }
          case Fetch(ref) => (s, DirtyThings.empty[K], s.fetch(ref).point[K])
          case FetchVector(refs) => (s, DirtyThings.empty[K], s.fetchVector(refs).point[K])
          case WhenResolved(ref, f) => s.addDomainResolutionTrigger(ref, f) match {
            case (s1, ok) => (s1, DirtyThings.empty[K], ok.getOrElse(().point[K]))
          }
        }
      }

      def uncons[K[_]: Applicative](w: DirtyThings[K])(s: Domains[K]): Option[(K[Unit], DirtyThings[K], Domains[K])] = w.uncons match {
        case None => None
        case Some((dt, dts)) => dt match {
          case DirtyDomain(ref) => s.triggersForDomain(ref) match {
            case (s1, Nil) => Some((Applicative[K].pure(()), dts, s1))
            case (s1, ks) => Some((Foldable[List].sequence_(ks), dts, s1))
          }
          case DirtySel(sel) => s.triggersForSel(sel) match {
            case (s1, Nil) => Some((Applicative[K].pure(()), dts, s1))
            case (s1, ks) => Some((Foldable[List].sequence_(ks), dts, s1))
          }
        }
      }
    }
}