package nutcracker.util

import scalaz.Ordering

trait HOrderK[F[_]] extends HEqualK[F] with OrderK[F] {
  def hOrderK[A, B](fa: F[A], fb: F[B]): Ordering

  override def orderK[A](f1: F[A], f2: F[A]): Ordering =
    hOrderK(f1, f2)

  override def hEqualK[A, B](fa: F[A], fb: F[B]): Boolean =
    hOrderK(fa, fb) == Ordering.EQ
}