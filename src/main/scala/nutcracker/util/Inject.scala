package nutcracker.util

import scalaz.{-\/, Coproduct, \/-, ~>}

// Like scalaz.Inject, but that one is sealed, so we can't
// create it as a specialization of InjectK.
trait Inject[F[_], G[_]] extends (F ~> G) { self =>
  def inj[A](fa: F[A]): G[A]

  def apply[A](fa: F[A]): G[A] = inj(fa)

  def andThen[H[_]](implicit that: Inject[G, H]): Inject[F, H] = new Inject[F, H] {
    def inj[A](fa: F[A]) = that(self(fa))
  }

  def compose[E[_]](implicit that: Inject[E, F]): Inject[E, G] =
    that andThen this
}

object Inject extends InjectInstances0 {
  def apply[F[_], G[_]](implicit inj: Inject[F, G]): Inject[F, G] = inj
}

trait InjectInstances0 extends InjectInstances1 {
  implicit def reflexiveInject[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      def inj[A](fa: F[A]): F[A] = fa
    }

  implicit def injectLeft[F[_], G[_]]: Inject[F, Coproduct[F, G, ?]] =
    new Inject[F, Coproduct[F, G, ?]] {
      def inj[A](fa: F[A]): Coproduct[F, G, A] = Coproduct(-\/(fa))
    }

  implicit def injectRight[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, ?]] =
    new Inject[F, Coproduct[H, G, ?]] {
      def inj[A](fa: F[A]): Coproduct[H, G, A] = Coproduct(\/-(I.inj(fa)))
    }
}

trait InjectInstances1 extends InjectInstances2 {
  implicit def injectCoproduct[F[_], G[_], H[_]](implicit
    injF: Inject[F, H],
    injG: Inject[G, H]
  ): Inject[Coproduct[F, G, ?], H] = new Inject[Coproduct[F, G, ?], H] {
    def inj[A](ca: Coproduct[F, G, A]): H[A] = ca.run match {
      case -\/(fa) => injF.inj(fa)
      case \/-(ga) => injG.inj(ga)
    }
  }
}

trait InjectInstances2 {
  implicit def specialize[F[_[_], _], G[_[_], _], K[_]](implicit inj: InjectK[F, G]): Inject[F[K, ?], G[K, ?]] = inj[K]
}