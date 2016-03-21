package nutcracker.util

import scala.language.higherKinds
import scalaz.~>

trait FunctorK[F[_[_]]] {

  def transform[K[_], L[_]](fk: F[K])(f: K ~> L): F[L]

}

object FunctorK {
  def apply[F[_[_]]](implicit fk: FunctorK[F]): FunctorK[F] = fk
}