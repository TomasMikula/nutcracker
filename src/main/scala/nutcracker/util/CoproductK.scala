package nutcracker.util

import scala.language.higherKinds
import scalaz.{-\/, \/, \/-, ~>}

final case class CoproductK[F[_[_], _], G[_[_], _], K[_], A](run: F[K, A] \/ G[K, A])

object CoproductK {
  def leftc[F[_[_], _], G[_[_], _], K[_], A](fa: F[K, A]): CoproductK[F, G, K, A] =
    CoproductK(-\/(fa))

  def rightc[F[_[_], _], G[_[_], _], K[_], A](ga: G[K, A]): CoproductK[F, G, K, A] =
    CoproductK(\/-(ga))

  def transform[F[_[_], _], G[_[_], _], K[_], M[_]](fm: F[K, ?] ~> M, gm: G[K, ?] ~> M): CoproductK[F, G, K, ?] ~> M =
    new (CoproductK[F, G, K, ?] ~> M) {
      def apply[A](ca: CoproductK[F, G, K, A]): M[A] = ca.run match {
        case -\/(fa) => fm(fa)
        case \/-(ga) => gm(ga)
      }
    }

  implicit def functorKInstance[F[_[_], _], G[_[_], _]](implicit
    FK: FunctorKA[F],
    GK: FunctorKA[G]
  ): FunctorKA[CoproductK[F, G, ?[_], ?]] =
    new FunctorKA[CoproductK[F, G, ?[_], ?]] {
      def transform[K[_], L[_], A](ck: CoproductK[F, G, K, A])(f: ~>[K, L]): CoproductK[F, G, L, A] = ck.run match {
        case -\/(fk) => leftc(FK.transform(fk)(f))
        case \/-(gk) => rightc(GK.transform(gk)(f))
      }
    }
}