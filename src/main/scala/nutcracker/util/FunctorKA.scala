package nutcracker.util

import scala.language.higherKinds
import scalaz.~>

trait FunctorKA[F[_[_], _]] {

  def transform[K[_], L[_], A](fk: F[K, A])(f: K ~> L): F[L, A]

}
