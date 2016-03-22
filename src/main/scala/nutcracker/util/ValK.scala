package nutcracker.util

import scala.language.higherKinds

trait ValK[F[_[_]]] { self =>
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[K[_]]: F[K]

  final def apply[K[_]]: F[K] = value.asInstanceOf[F[K]]

  final def map[G[_[_]]](tr: F ≈> G): ValK[G] = new ValK[G] {
    def compute[K[_]]: G[K] = tr(self.compute[K])
  }
}
