package nutcracker.util

import scala.language.higherKinds
import scalaz.Monoid

/** This is similar to `cats.MonoidK`,
  * except `F`'s kind is `(* -> *) -> *` instead of `* -> *`.
  */
trait MonoidK[F[_[_]]] { self =>

  def zero[K[_]]: F[K]
  def append[K[_]](f1: F[K], f2: F[K]): F[K]

  def monoid[K[_]]: Monoid[F[K]] = new Monoid[F[K]] {
    def zero: F[K] = self.zero
    def append(f1: F[K], f2: => F[K]): F[K] = self.append(f1, f2)
  }
}
