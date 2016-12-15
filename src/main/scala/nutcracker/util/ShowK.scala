package nutcracker.util

import scalaz.~>

trait ShowK[F[_]] extends (F ~> λ[α => String]) {
  def shows[A](fa: F[A]): String

  def apply[A](fa: F[A]): String = shows(fa)
}

object ShowK {
  def fromToString[F[_]]: ShowK[F] = new ShowK[F] {
    def shows[A](fa: F[A]): String = fa.toString
  }
}