package nutcracker.util

trait ShowK[F[_]] {
  def shows[A](fa: F[A]): String
}

object ShowK {
  def fromToString[F[_]]: ShowK[F] = new ShowK[F] {
    def shows[A](fa: F[A]): String = fa.toString
  }
}