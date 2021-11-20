package nutcracker.util.typealigned

import scala.language.higherKinds
import scalaz.Compose

/**
 * Has the `map` operation of a functor.
 * When `=>:` forms a category (on Scala types), then `FunctorLike`
 * is a functor if it satisfies the functor laws.
 */
trait FunctorLike[F[_], =>:[_, _]] {
  type :<=[A, B] = B =>: A

  def map[A, B](fa: F[A])(f: A =>: B): F[B]

  /** View this covariant functor as a contravariant functor in the opposite category. */
  def contramap[A, B](fa: F[A])(f: B :<= A): F[B]
}

object FunctorLike {

  trait FromCovariant[F[_], =>:[_, _]] extends FunctorLike[F, =>:] {
    def contramap[A, B](fa: F[A])(f: B :<= A): F[B] = map(fa)(f)
  }

  trait FromContravariant[F[_], =>:[_, _]] extends FunctorLike[F, Op[=>:, *, *]] {
    def map[A, B](fa: F[A])(f: B =>: A): F[B] = contramap(fa)(f)
  }

  implicit def covariantSecond[=>:[_, _], X](implicit ev: Compose[=>:]): FunctorLike[X =>: *, =>:] =
    new FromCovariant[X =>: *, =>:] {
      def map[A, B](fa: X =>: A)(f: A =>: B): X =>: B =
        ev.compose(f, fa)
    }

  implicit def contravariantFirst[=>:[_, _], X](implicit ev: Compose[=>:]): ContravariantLike[* =>: X, =>:] =
    new FromContravariant[* =>: X, =>:] {
      def contramap[A, B](fa: A =>: X)(f: B =>: A): B =>: X =
        ev.compose(fa, f)
    }
}
