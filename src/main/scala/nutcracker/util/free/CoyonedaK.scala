package nutcracker.util.free

import scala.language.higherKinds
import scalaz.{~>, Functor}

sealed trait CoyonedaK[F[_[_], _], K[_], A] {

}

object CoyonedaK {
  final case class Pure[F[_[_], _], K[_], A](fa: F[K, A]) extends CoyonedaK[F, K, A]
  final case class Map[F[_[_], _], K[_], A, B](fa: F[K, A], f: A => B) extends CoyonedaK[F, K, B]

  implicit def functorInstance[F[_[_], _], K[_]]: Functor[CoyonedaK[F, K, ?]] = new Functor[CoyonedaK[F, K, ?]] {
    def map[A, B](fa: CoyonedaK[F, K, A])(f: (A) => B): CoyonedaK[F, K, B] = fa match {
      case Pure(a) => Map(a, f)
      case Map(a, g) => Map(a, f compose g)
    }
  }

  implicit def functorKInstance[F[_[_], _]](implicit FK: FunctorKA[F]): FunctorKA[CoyonedaK[F, ?[_], ?]] =
    new FunctorKA[CoyonedaK[F, ?[_], ?]] {
      def transform[K[_], L[_], A](fk: CoyonedaK[F, K, A])(f: K ~> L): CoyonedaK[F, L, A] = fk match {
        case Pure(a) => Pure(FK.transform(a)(f))
        case Map(a, g) => Map(FK.transform(a)(f), g)
      }
    }
}