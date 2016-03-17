package nutcracker.util

import scala.language.higherKinds

trait ValK[F[_[_]]] {
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[K[_]]: F[K]

  final def apply[K[_]]: F[K] = value.asInstanceOf[F[K]]
}
