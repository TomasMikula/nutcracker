package nutcracker

import scala.language.higherKinds

package object util {
  type ConstK[A, K[_]] = A

  type ≈>[F[_[_]], G[_[_]]] = TransformK[F, G]
  type ~~>[F[_[_], _], G[_[_], _]] = TransformKA[F, G]
  type ~>>[F[_[_], _], G[_]] = F ~~> λ[(K[_], A) => G[A]]

  type Index[K, V] = TransformedIndex[K, V, V]

  object Index {
    def empty[K, V](f: V => Seq[K]): Index[K, V] = TransformedIndex.empty(f, (v, k) => v)
  }

}
