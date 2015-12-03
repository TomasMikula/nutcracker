package nutcracker.util.free

import scala.language.higherKinds

trait InjectK[F[_, _[_]], H[_, _[_]]] {
  def inj[A, K[_]](fa: F[A, K]): H[A, K]
  def prj[A, K[_]](ha: H[A, K]): Option[F[A, K]]
}
