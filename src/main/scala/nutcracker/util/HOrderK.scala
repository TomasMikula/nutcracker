package nutcracker.util

import algebra.Order

trait HOrderK[F[_]] extends Order[∃[F]] {
  def hOrder[A, B](fa: F[A], fb: F[B]): Int

  override def compare(x: ∃[F], y: ∃[F]) = hOrder(x, y)
}