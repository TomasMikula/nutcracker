package nutcracker

import shapeless.HList

import scala.language.existentials
import PartialSolution._
import scalaz.Monoid

case class DirtyThings(
    continuations: List[ProblemDescription[Unit]],
    domains: Set[CellRef[D] forSome { type D }],
    selections: Set[Sel[_ <: HList]]
) {
  def split: Option[(DirtyThing, DirtyThings)] =
    if(continuations.nonEmpty) Some((PendingContinuation(continuations.head), this.copy(continuations = continuations.tail)))
    else if(domains.nonEmpty) Some((DirtyDomain(domains.head), this.copy(domains = domains.tail)))
    else if(selections.nonEmpty) Some((DirtySel(selections.head), this.copy(selections = selections.tail)))
    else None
}

object DirtyThings {
  def empty: DirtyThings = DirtyThings(Nil, Set(), Set())

  implicit def monoid: Monoid[DirtyThings] = new Monoid[DirtyThings] {
    def zero: DirtyThings = DirtyThings.empty
    def append(x: DirtyThings, y: => DirtyThings): DirtyThings = DirtyThings(
        x.continuations ++ y.continuations,
        x.domains ++ y.domains,
        x.selections ++ y.selections)
  }

  def dirtyDomain[D](ref: CellRef[D]): DirtyThings = DirtyThings(Nil, Set(ref), Set())
  def dirtyDomains[D](refs: Iterable[CellRef[D]]): DirtyThings = DirtyThings(Nil, refs.toSet, Set())
  def dirtySel(sel: Sel[_ <: HList]): DirtyThings = DirtyThings(Nil, Set(), Set(sel))
  def dirtySels(sels: Set[Sel[_ <: HList]]): DirtyThings = DirtyThings(Nil, Set(), sels)
  def continuation(cont: ProblemDescription[Unit]): DirtyThings = DirtyThings(List(cont), Set(), Set())
  def continuations(conts: List[ProblemDescription[Unit]]): DirtyThings = DirtyThings(conts, Set(), Set())
}

sealed trait DirtyThing
case class PendingContinuation(cont: ProblemDescription[Unit]) extends DirtyThing
case class DirtyDomain[D](ref: CellRef[D]) extends DirtyThing
case class DirtySel(sel: Sel[_ <: HList]) extends DirtyThing