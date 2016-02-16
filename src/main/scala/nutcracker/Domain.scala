package nutcracker

import algebra.lattice.{GenBool, MeetSemilattice}

trait Domain[A, D] extends MeetSemilattice[D] {
  def values(d: D): Domain.Values[A, D]
  def sizeUpperBound(d: D): Option[Long]
  def singleton(a: A): D

  /**
    * Non-commutative version of `meet`, which returns a new value only if
    * progress was made, i.e. if `meet(d, by) != d`
    * @param d value to refine
    * @param by value to refine by
    * @return `Some(d ∧ by)` if `d ∧ by < d`, `None` otherwise.
    */
  def refine(d: D, by: D): Option[D]

  def isEmpty(d: D): Boolean = sizeUpperBound(d).map(_ == 0).getOrElse(false)
}

object Domain {
  def apply[A, D: Domain[A, ?]]: Domain[A, D] = implicitly[Domain[A, D]]

  sealed trait Values[A, D]
  case class Empty[A, D]() extends Values[A, D]
  case class Just[A, D](value: A) extends Values[A, D]
  case class Many[A, D](branchings: Stream[List[D]]) extends Values[A, D]

  implicit def finiteSetDomain[A]: Domain[A, Set[A]] = new Domain[A, Set[A]] {
    def meet(a: Set[A], b: Set[A]): Set[A] = a intersect b
    def singleton(a: A): Set[A] = Set(a)
    def sizeUpperBound(d: Set[A]): Option[Long] = Some(d.size.toLong)
    def values(d: Set[A]): Values[A, Set[A]] = d.size match {
      case 0 => Empty()
      case 1 => Just(d.head)
      case _ => Many(Stream(d.toList map (Set(_)))) // split into singleton sets
    }
    def refine(d: Set[A], by: Set[A]): Option[Set[A]] = {
      val res = d intersect by
      if(res.size < d.size) Some(res)
      else None
    }
  }

  implicit def setGenBool[A]: GenBool[Set[A]] = algebra.std.set.setLattice
}