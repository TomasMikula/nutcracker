package nutcracker.util

import scalaz.~>

trait ShowK[F[_]] extends (F ~> λ[α => String]) {
  def shows[A](fa: F[A]): String

  def apply[A](fa: F[A]): String = shows(fa)

  final def aggregator[A](implicit ev: Aggregator[A, String]): AggregatorK[A, F] =
    AggregatorK.byConst(this)
}

object ShowK {
  def fromToString[F[_]]: ShowK[F] = new ShowK[F] {
    def shows[A](fa: F[A]): String = fa.toString
  }
}