package nutcracker.util

import scala.language.higherKinds

package object free {
  type ConstK[A, K[_]] = A

  type ~~>[F[_[_], _], G[_[_], _]] = TransformKA[F, G]
  type ~>>[F[_[_], _], G[_]] = F ~~> λ[(K[_], A) => G[A]]
}
