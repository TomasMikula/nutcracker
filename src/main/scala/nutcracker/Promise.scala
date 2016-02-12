package nutcracker

import algebra.Eq
import algebra.lattice.BoundedMeetSemilattice
import nutcracker.Domain.Values

sealed trait Promise[+A]

object Promise {

  private final case object Empty extends Promise[Nothing] // incomplete promise
  private final case class Completed[A](value: A) extends Promise[A]
  private final case object Contradiction extends Promise[Nothing] // promise completed multiple times with different values

  implicit def promiseLattice[A](implicit EqA: Eq[A]): Domain[A, Promise[A]] with BoundedMeetSemilattice[Promise[A]] = new Domain[A, Promise[A]] with BoundedMeetSemilattice[Promise[A]] {
    def values(pa: Promise[A]): Values[A, Promise[A]] = pa match {
      case Empty => Domain.Many(Stream.empty)
      case Completed(a) => Domain.Just(a)
      case Contradiction => Domain.Empty()
    }

    def sizeUpperBound(p: Promise[A]): Option[Long] = Some(p match {
      case Contradiction => 0
      case _ => 1
    })

    def singleton(a: A): Promise[A] = Completed(a)

    def eqv(x: Promise[A], y: Promise[A]): Boolean = (x, y) match {
      case (Empty, Empty) => true
      case (Contradiction, Contradiction) => true
      case (Completed(a), Completed(b)) => EqA.eqv(a, b)
      case _ => false
    }

    def meet(x: Promise[A], y: Promise[A]): Promise[A] = (x, y) match {
      case (Empty, p) => p
      case (p, Empty) => p
      case (Completed(a), Completed(b)) if EqA.eqv(a, b) => x
      case _ => Contradiction
    }

    def one: Promise[A] = Empty
  }
}