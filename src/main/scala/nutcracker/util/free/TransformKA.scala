package nutcracker.util.free

import scala.language.higherKinds

import scalaz.~>

trait TransformKA[F[_[_], _], G[_[_], _]] {
  import nutcracker.util.free.TransformKA._

  def apply[K[_], A](f: F[K, A]): G[K, A]
  def papply[K[_]]: F[K, ?] ~> G[K, ?] = PAppliedTransformKA(this)
}

object TransformKA {
  private case class PAppliedTransformKA[F[_[_], _], G[_[_], _], K[_]](run: F ~~> G) extends (F[K, ?] ~> G[K, ?]) {
    def apply[A](f: F[K, A]): G[K, A] = run(f)
  }

  implicit def idTransform[F[_[_], _]]: F ~~> F = new (F ~~> F) {
    def apply[K[_], A](f: F[K, A]): F[K, A] = f
  }
}