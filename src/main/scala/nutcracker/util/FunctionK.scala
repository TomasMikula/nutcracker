package nutcracker.util

import scala.language.higherKinds

/**
  * Universally quantified function:
  * `∀ K[_]. F[K] => G[K]`
  */
trait FunctionK[F[_[_]], G[_[_]]] {

  def apply[K[_]](f: F[K]): G[K]
}
