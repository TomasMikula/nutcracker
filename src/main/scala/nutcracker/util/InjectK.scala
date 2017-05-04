package nutcracker.util

import scalaz.{-\/, \/-}

trait InjectK[F[_[_], _], H[_[_], _]] extends (F ≈~> H) { self =>
  def inj[K[_], A](fa: F[K, A]): H[K, A]

  def apply[K[_], A](fa: F[K, A]): H[K, A] = inj(fa)

  def andThen[I[_[_], _]](implicit that: InjectK[H, I]): InjectK[F, I] = new InjectK[F, I] {
    def inj[K[_], A](fa: F[K, A]) = that.inj(InjectK.this.inj(fa))
  }

  def compose[E[_[_], _]](implicit that: InjectK[E, F]): InjectK[E, H] =
    that andThen this

  def apply[K[_]]: Inject[F[K, ?], H[K, ?]] = new Inject[F[K, ?], H[K, ?]] {
    def inj[A](fa: F[K, A]): H[K, A] = self.inj(fa)
  }
}

object InjectK extends InjectKInstances0 {
  def apply[F[_[_], _], H[_[_], _]](implicit inj: InjectK[F, H]): InjectK[F, H] = inj
}

trait InjectKInstances0 extends InjectKInstances1 {
  implicit def reflexiveInject[F[_[_], _]]: InjectK[F, F] =
    new InjectK[F, F] {
      def inj[K[_], A](fa: F[K, A]): F[K, A] = fa
    }

  implicit def injectLeft[F[_[_], _], G[_[_], _]]: InjectK[F, CoproductK[F, G, ?[_], ?]] =
    new InjectK[F, CoproductK[F, G, ?[_], ?]] {
      def inj[K[_], A](fa: F[K, A]): CoproductK[F, G, K, A] = CoproductK(-\/(fa))
    }

  implicit def injectRight[F[_[_], _], G[_[_], _], H[_[_], _]](implicit I: InjectK[F, G]): InjectK[F, CoproductK[H, G, ?[_], ?]] =
    new InjectK[F, CoproductK[H, G, ?[_], ?]] {
      def inj[K[_], A](fa: F[K, A]): CoproductK[H, G, K , A] = CoproductK(\/-(I.inj(fa)))
    }
}

trait InjectKInstances1 {
  implicit def injectCoproduct[F[_[_], _], G[_[_], _], H[_[_], _]](implicit
    injF: InjectK[F, H],
    injG: InjectK[G, H]
  ): InjectK[CoproductK[F, G, ?[_], ?], H] = new InjectK[CoproductK[F, G, ?[_], ?], H] {
    def inj[K[_], A](ca: CoproductK[F, G, K, A]): H[K, A] = ca.run match {
      case -\/(fa) => injF.inj(fa)
      case \/-(ga) => injG.inj(ga)
    }
  }
}