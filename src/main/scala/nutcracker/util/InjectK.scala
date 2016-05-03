package nutcracker.util

import scala.language.{higherKinds, implicitConversions}
import scalaz.{-\/, \/-}

trait InjectK[F[_[_], _], H[_[_], _]] extends (F ~~> H) {
  def apply[K[_], A](fa: F[K, A]): H[K, A] = inj(fa)
  def inj[K[_], A](fa: F[K, A]): H[K, A]
  def prj[K[_], A](ha: H[K, A]): Option[F[K, A]]
}

object InjectK extends InjectK0 {

  implicit def reflexiveInject[F[_[_], _]]: InjectK[F, F] =
    new InjectK[F, F] {
      def inj[K[_], A](fa: F[K, A]): F[K, A] = fa
      def prj[K[_], A](ha: F[K, A]): Option[F[K, A]] = ???
    }

  implicit def injectLeft[F[_[_], _], G[_[_], _]]: InjectK[F, CoproductK[F, G, ?[_], ?]] =
    new InjectK[F, CoproductK[F, G, ?[_], ?]] {
      def inj[K[_], A](fa: F[K, A]): CoproductK[F, G, K, A] = CoproductK(-\/(fa))
      def prj[K[_], A](ha: CoproductK[F, G, K, A]): Option[F[K, A]] = ???
    }

  implicit def injectRight[F[_[_], _], G[_[_], _], H[_[_], _]](implicit I: InjectK[F, G]): InjectK[F, CoproductK[H, G, ?[_], ?]] =
    new InjectK[F, CoproductK[H, G, ?[_], ?]] {
      def inj[K[_], A](fa: F[K, A]): CoproductK[H, G, K , A] = CoproductK(\/-(I.inj(fa)))
      def prj[K[_], A](ga: CoproductK[H, G, K, A]) = ???
    }

  implicit def injectCoyo[F[_[_], _], G[_[_], _]](implicit I: InjectK[F, G]): InjectK[F, CoyonedaK[G, ?[_], ?]] =
    new InjectK[F, CoyonedaK[G, ?[_], ?]] {
      def inj[K[_], A](fa: F[K, A]): CoyonedaK[G, K, A] = CoyonedaK.Pure(I.inj(fa))
      def prj[K[_], A](ha: CoyonedaK[G, K, A]): Option[F[K, A]] = ???
    }

  implicit def lift[F[_[_], _], G[_[_], _], K[_], A](f: F[K, A])(implicit inj: InjectK[F, G]): G[K, A] =
    inj.inj(f)
}

trait InjectK0 {
  implicit def injectCoproduct[F[_[_], _], G[_[_], _], H[_[_], _]](implicit
    injF: InjectK[F, H],
    injG: InjectK[G, H]
  ): InjectK[CoproductK[F, G, ?[_], ?], H] = new InjectK[CoproductK[F, G, ?[_], ?], H] {
    def inj[K[_], A](ca: CoproductK[F, G, K, A]): H[K, A] = ca.run match {
      case -\/(fa) => injF.inj(fa)
      case \/-(ga) => injG.inj(ga)
    }
    def prj[K[_], A](ha: H[K, A]): Option[CoproductK[F, G, K, A]] = ???
  }
}