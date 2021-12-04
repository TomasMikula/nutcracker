package nutcracker.util

import scalaz.{Order, Ordering}

trait OrderK[F[_]] extends EqualK[F] {
  def orderK[A](f1: F[A], f2: F[A]): Ordering

  override def equalK[A](f1: F[A], f2: F[A]): Boolean =
    orderK(f1, f2) == Ordering.EQ

  override def specialize[A]: Order[F[A]] =
    new Order[F[A]] {
      override def order(x: F[A], y: F[A]): Ordering =
        orderK(x, y)
    }
}
