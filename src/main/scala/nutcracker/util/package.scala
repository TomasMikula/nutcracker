package nutcracker

import scala.language.higherKinds
import scalaz.Id.Id
import scalaz.{Applicative, ~>}

package object util {
  type ConstK[A, K[_]] = A

  type ≈>[F[_[_]], G[_[_]]] = TransformK[F, G]
  type ~~>[F[_[_], _], G[_[_], _]] = TransformKA[F, G]
  type ~>>[F[_[_], _], G[_]] = F ~~> λ[(K[_], A) => G[A]]

  type Index[K, V] = TransformedIndex[K, V, V]

  object Index {
    def empty[K, V](f: V => Seq[K]): Index[K, V] =
      TransformedIndex.empty(f, (v, k) => v)
  }

  type WriterState[W, S, A] = WriterStateT[Id, W, S, A]
  object WriterState {
    def apply[W, S, A](run: S => (W, S, A)): WriterState[W, S, A] =
      WriterStateT[Id, W, S, A](run)
  }

  def idToM[M[_]](implicit M: Applicative[M]): Id ~> M = new (Id ~> M) {
    def apply[A](a: Id[A]): M[A] = M.point(a)
  }
}
