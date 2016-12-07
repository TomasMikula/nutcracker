package nutcracker.util

trait ShowK[F[_]] {
  def shows[A](fa: F[A]): String
}
