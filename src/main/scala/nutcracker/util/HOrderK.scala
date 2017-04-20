package nutcracker.util

import scalaz.{Order, Ordering}

trait HOrderK[F[_]] extends HEqualK[F] with Order[∃[F]] {
  def hOrderK[A, B](fa: F[A], fb: F[B]): Ordering

  override def order(x: ∃[F], y: ∃[F]) = hOrderK(x, y)
  override def hEqualK[A, B](fa: F[A], fb: F[B]): Boolean = hOrderK(fa, fb) == Ordering.EQ
  override def equal(x: ∃[F], y: ∃[F]): Boolean = hOrderK(x, y) == Ordering.EQ
}