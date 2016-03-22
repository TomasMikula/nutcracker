package nutcracker.util

import scala.language.higherKinds

trait TransformK[F[_[_]], G[_[_]]] {

  def apply[K[_]](f: F[K]): G[K]
}
