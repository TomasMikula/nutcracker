package nutcracker.util

import scalaz.{Order, Ordering}

trait HOrderK[F[_]] extends Order[∃[F]] {
  def hOrder[A, B](fa: F[A], fb: F[B]): Ordering

  override def order(x: ∃[F], y: ∃[F]) = hOrder(x, y)
}