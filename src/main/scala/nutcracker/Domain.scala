package nutcracker

import algebra.Eq
import algebra.lattice.MeetSemilattice

trait Domain[A, D] extends Eq[D] with MeetSemilattice[D] {
  def values(d: D): Domain.Values[A, D]
  def sizeUpperBound(d: D): Option[Long]
  def singleton(a: A): D

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
      case _ => Many(Stream(d.splitAt(d.size / 2) match { case (b1, b2) => b1 :: b2 :: Nil }))
    }
    def eqv(x: Set[A], y: Set[A]): Boolean = x == y // XXX uses universal equality of A
  }
}