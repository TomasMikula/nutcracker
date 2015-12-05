package nutcracker.util.free

import scala.language.higherKinds

trait InjectK[F[_[_], _], H[_[_], _]] {
  def inj[K[_], A](fa: F[K, A]): H[K, A]
  def prj[K[_], A](ha: H[K, A]): Option[F[K, A]]
}
