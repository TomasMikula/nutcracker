package nutcracker

import algebra.Eq
import algebra.lattice.BoundedMeetSemilattice
import nutcracker.Domain.Values

sealed trait Promise[+A]

object Promise {

  private final case object Empty extends Promise[Nothing] // incomplete promise
  private final case class Completed[A](value: A) extends Promise[A]
  private final case object Contradiction extends Promise[Nothing] // promise completed multiple times

  implicit def promiseLattice[A]: Domain[A, Promise[A]] with BoundedMeetSemilattice[Promise[A]] = new Domain[A, Promise[A]] with BoundedMeetSemilattice[Promise[A]] {
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

    /** Completed promise cannot be completed again. Therefore, any attempt
      * to refine a completed promise will result in bottom, even if refining
      * with the exact same value. Note that this breaks the idempotence of
      * `meet`, but it allows us to not require `Eq` instance on `A`.
      */
    def refine(x: Promise[A], y: Promise[A]): Option[Promise[A]] = (x, y) match {
      case (_, Empty) => None
      case (Empty, p) => Some(p)
      case (Completed(a), _) => Some(Contradiction)
      case _ => None
    }

    /** Completed promise cannot be completed again. Therefore, any attempt
      * to refine a completed promise will result in bottom, even if refining
      * with the exact same value. Note that this breaks the idempotence of
      * `meet`, but it allows us to not require `Eq` instance on `A`.
      */
    def meet(x: Promise[A], y: Promise[A]): Promise[A] = (x, y) match {
      case (Empty, p) => p
      case (p, Empty) => p
      case _ => Contradiction
    }

    def one: Promise[A] = Empty
  }
}