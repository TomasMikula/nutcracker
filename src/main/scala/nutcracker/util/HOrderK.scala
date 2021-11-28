package nutcracker.util

import scalaz.{Order, Ordering}

trait HOrderK[F[_]] extends HEqualK[F] with Order[F[_]] {
  def hOrderK[A, B](fa: F[A], fb: F[B]): Ordering

  override def order(x: F[_], y: F[_]): Ordering = {
    // unsafe casts needed because of https://github.com/lampepfl/dotty/issues/14008
    hOrderK(
      x.asInstanceOf[F[Any]],
      y.asInstanceOf[F[Any]],
    )
  }

  override def hEqualK[A, B](fa: F[A], fb: F[B]): Boolean = hOrderK(fa, fb) == Ordering.EQ
  override def equal(x: F[_], y: F[_]): Boolean = order(x, y) == Ordering.EQ
}