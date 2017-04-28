package nutcracker.util

import scalaz.~>

// Like scalaz.Inject, but that one is sealed
trait Inject[F[_], G[_]] extends (F ~> G) {
  def inj[A](fa: F[A]): G[A]

  def apply[A](fa: F[A]): G[A] = inj(fa)
}

object Inject {
  implicit def specialize[F[_[_], _], G[_[_], _], K[_]](implicit inj: InjectK[F, G]): Inject[F[K, ?], G[K, ?]] = inj[K]
}