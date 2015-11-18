package nutcracker

import scala.language.existentials
import PartialSolution._
import scalaz.Monoid

case class DirtyThings(
    continuations: List[ProblemDescription[Unit]],
    domains: Set[PureDomRef[A, D] forSome { type A; type D }]
) {
  def split: Option[(DirtyThing, DirtyThings)] =
    if(continuations.nonEmpty) Some((PendingContinuation(continuations.head), this.copy(continuations = continuations.tail)))
    else if(domains.nonEmpty) Some((DirtyDomain(domains.head), this.copy(domains = domains.tail)))
    else None
}

object DirtyThings {
  def empty: DirtyThings = DirtyThings(Nil, Set())

  implicit def monoid: Monoid[DirtyThings] = new Monoid[DirtyThings] {
    def zero: DirtyThings = DirtyThings.empty
    def append(x: DirtyThings, y: => DirtyThings): DirtyThings = DirtyThings(
        x.continuations ++ y.continuations,
        x.domains ++ y.domains)
  }

  def dirtyDomain[A, D](ref: PureDomRef[A, D]): DirtyThings = DirtyThings(Nil, Set(ref))
  def continuation(cont: ProblemDescription[Unit]): DirtyThings = DirtyThings(List(cont), Set())
  def continuations(conts: List[ProblemDescription[Unit]]): DirtyThings = DirtyThings(conts, Set())
}

sealed trait DirtyThing
case class PendingContinuation(cont: ProblemDescription[Unit]) extends DirtyThing
case class DirtyDomain[A, D](ref: PureDomRef[A, D]) extends DirtyThing