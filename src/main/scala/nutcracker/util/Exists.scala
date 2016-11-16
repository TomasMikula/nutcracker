package nutcracker.util

import scala.language.higherKinds

sealed abstract class Exists[F[_]] {
  type X
  val value: F[X]
}

object Exists {
  def apply[F[_], A](fa: F[A]): Exists[F] = new Exists[F] {
    type X = A
    val value = fa
  }
}