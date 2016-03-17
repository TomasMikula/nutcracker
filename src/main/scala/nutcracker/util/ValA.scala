package nutcracker.util

import scala.language.higherKinds

trait ValA[F[_]] {
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[A]: F[A]

  final def apply[A]: F[A] = value.asInstanceOf[F[A]]
}
