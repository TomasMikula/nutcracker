package nutcracker.util

import scala.language.higherKinds
import scalaz.{-\/, \/, \/-, ~>}

final case class CoproductK[F[_[_], _], G[_[_], _], K[_], A](run: F[K, A] \/ G[K, A]) extends AnyVal

object CoproductK {

  sealed trait Builder {
    type Out[_[_], _]
  }

  sealed trait :++:[F[_[_], _], G[_[_], _]] extends Builder {
    type Out[K[_], A] = CoproductK[F, G, K, A]
  }

  sealed trait :+:[F[_[_], _], B <: Builder] extends Builder {
    type Out[K[_], A] = CoproductK[F, B#Out, K, A]
  }

  def leftc[F[_[_], _], G[_[_], _], K[_], A](fa: F[K, A]): CoproductK[F, G, K, A] =
    CoproductK(-\/(fa))

  def rightc[F[_[_], _], G[_[_], _], K[_], A](ga: G[K, A]): CoproductK[F, G, K, A] =
    CoproductK(\/-(ga))

  implicit def functorKInstance[F[_[_], _], G[_[_], _]](implicit
    FK: FunctorKA[F],
    GK: FunctorKA[G]
  ): FunctorKA[CoproductK[F, G, ?[_], ?]] =
    new FunctorKA[CoproductK[F, G, ?[_], ?]] {
      def transform[K[_], L[_], A](ck: CoproductK[F, G, K, A])(f: K ~> L): CoproductK[F, G, L, A] = ck.run match {
        case -\/(fk) => leftc(FK.transform(fk)(f))
        case \/-(gk) => rightc(GK.transform(gk)(f))
      }
    }
}
