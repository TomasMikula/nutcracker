package nutcracker.util

import scalaz.PlusEmpty

trait Catenable[F[_]] extends PlusEmpty[F] {
  def cons[A](a: A,  fa: F[A]): F[A]
  def singleton[A](a: A): F[A]
  def uncons[A](fa: F[A]): Option[(A, F[A])]
}