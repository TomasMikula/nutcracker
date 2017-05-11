package nutcracker.util

import nutcracker.util.free.Free
import scalaz.{-\/, Coproduct, \/-, ~>}

/** Similar to [[scalaz.Inject]], except:
  *  - not sealed, thus not restricted to injections into  [[scalaz.Coproduct]];
  *  - doesn't require to implement projection, so more instances are possible.
  */
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
  implicit def injectLeft[F[_], G[_]]: Inject[F, Coproduct[F, G, ?]] =
    new Inject[F, Coproduct[F, G, ?]] {
      def inj[A](fa: F[A]): Coproduct[F, G, A] = Coproduct(-\/(fa))
    }

  implicit def injectRight[F[_], G[_]]: Inject[G, Coproduct[F, G, ?]] =
    new Inject[G, Coproduct[F, G, ?]] {
      def inj[A](ga: G[A]): Coproduct[F, G, A] = Coproduct(\/-(ga))
    }
}

trait InjectInstances1 extends InjectInstances2 {
  implicit def injectLeftRec[F[_], G[_], H[_]](implicit fg: Inject[F, G]): Inject[F, Coproduct[G, H, ?]] =
    new Inject[F, Coproduct[G, H, ?]] {
      def inj[A](fa: F[A]): Coproduct[G, H, A] = Coproduct(-\/(fg(fa)))
    }

  implicit def injectRightRec[F[_], G[_], H[_]](implicit fh: Inject[F, H]): Inject[F, Coproduct[G, H, ?]] =
    new Inject[F, Coproduct[G, H, ?]] {
      def inj[A](fa: F[A]): Coproduct[G, H, A] = Coproduct(\/-(fh(fa)))
    }
}

trait InjectInstances2 extends InjectInstances3 {
  implicit def freeLift[F[_]]: Inject[F, Free[F, ?]] =
    λ[Inject[F, Free[F, ?]]].inj(fa => Free.liftF(fa))

  implicit def freeKLift[F[_[_], _]]: Inject[F[FreeK[F, ?], ?], FreeK[F, ?]] =
    λ[Inject[F[FreeK[F, ?], ?], FreeK[F, ?]]].inj(fa => FreeK.liftF(fa))
}

trait InjectInstances3 extends InjectInstances4 {
  implicit def reflexiveInject[F[_]]: Inject[F, F] =
    new Inject[F, F] {
      def inj[A](fa: F[A]): F[A] = fa
    }
}

trait InjectInstances4 extends InjectInstances5 {
  implicit def transitiveInject[F[_], G[_], H[_]](implicit fg: Inject[F, G], gh: Inject[G, H]): Inject[F, H] =
    fg andThen gh
}

trait InjectInstances5 extends InjectInstances6 {
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

trait InjectInstances6 {
  implicit def specialize[F[_[_], _], G[_[_], _], K[_]](implicit inj: InjectK[F, G]): Inject[F[K, ?], G[K, ?]] = inj[K]
}