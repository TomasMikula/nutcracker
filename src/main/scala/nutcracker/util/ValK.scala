package nutcracker.util

import scala.language.higherKinds

/** Universally quantified value:
  * `∀ K[_]. F[K]`
  */
trait ValK[F[_[_]]] { self =>
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[K[_]]: F[K]

  final def apply[K[_]]: F[K] = value.asInstanceOf[F[K]]

  /** Transform this value by a universally quantified function
    * `f: ∀ K[_]. F[K] => G[K]`
    */
  final def transform[G[_[_]]](f: F ≈> G): ValK[G] = new ValK[G] {
    def compute[K[_]]: G[K] = f(self.compute[K])
  }
}
