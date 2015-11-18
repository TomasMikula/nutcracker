package nutcracker

import algebra.Eq
import algebra.lattice.MeetSemilattice
import nutcracker.algebraic.BoolRng

trait Domain[A, D] extends Eq[D] with MeetSemilattice[D] {
  def values(d: D): Domain.Values[A, D]
  def sizeUpperBound(d: D): Option[Long]
  def singleton(a: A): D

  def isEmpty(d: D): Boolean = sizeUpperBound(d).map(_ == 0).getOrElse(false)
}

object Domain {
  sealed trait Values[A, D]
  case class Empty[A, D]() extends Values[A, D]
  case class Just[A, D](value: A) extends Values[A, D]
  case class Many[A, D](branchings: Stream[List[D]]) extends Values[A, D]

  implicit def setDomain[A]: Domain[A, Set[A]] = new Domain[A, Set[A]] with BoolRng[Set[A]] {
    def and(a: Set[A], b: Set[A]): Set[A] = a intersect b
    def or(a: Set[A], b: Set[A]): Set[A] = a union b
    def setMinus(a: Set[A], b: Set[A]): Set[A] = a diff b
    def xor(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
    def zero: Set[A] = Set.empty
    def singleton(a: A): Set[A] = Set(a)
    def sizeUpperBound(d: Set[A]): Option[Long] = Some(d.size.toLong)
    def values(d: Set[A]): Values[A, Set[A]] = d.size match {
      case 0 => Empty()
      case 1 => Just(d.head)
      case _ => Many(Stream(d.splitAt(d.size / 2) match { case (b1, b2) => b1 :: b2 :: Nil }))
    }
    def eqv(x: Set[A], y: Set[A]): Boolean = x == y // XXX uses universal equality of A
  }

  def equal[A]: Constraint[(Set[A], Set[A])] = new Constraint[(Set[A], Set[A])] {
    def enforce(ab: (Set[A], Set[A])): (Set[A], Set[A]) = {
      val intersection = ab._1 intersect ab._2
      (intersection, intersection)
    }
  }
}