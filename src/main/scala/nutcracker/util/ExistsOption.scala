package nutcracker.util

sealed trait ExistsOption[F[_]] {
  def isEmpty: Boolean =
    this match {
      case ENone()  => true
      case ESome(_) => false
    }
  def unsafeGet: Exists[F] =
    this match {
      case ESome(fa) => Exists(fa)
      case ENone()   => throw new NoSuchElementException()
    }
}

case class ESome[F[_], A](value: F[A]) extends ExistsOption[F]

case class ENone[F[_]]() extends ExistsOption[F]

object ExistsOption {
  def fromOption[F[_], A](x: Option[F[A]]): ExistsOption[F] =
    x match {
      case Some(fa) => ESome(fa)
      case None     => ENone()
    }
}
