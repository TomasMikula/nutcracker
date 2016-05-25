package nutcracker.util

import scala.language.higherKinds
import scalaz.~>

/** Universally quantified value:
  * `∀ A. F[A]`
  */
trait ValA[F[_]] { self =>
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[A]: F[A]

  final def apply[A]: F[A] = value.asInstanceOf[F[A]]

  /** Transform this value by a universally quantified function
    * `f: ∀ A. F[A] => G[A]`
    */
  final def transform[G[_]](f: F ~> G): ValA[G] = new ValA[G] {
    def compute[A]: G[A] = f(self.compute[A])
  }
}
