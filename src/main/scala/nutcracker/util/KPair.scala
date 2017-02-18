package nutcracker.util

import scala.language.higherKinds
import scalaz.{Lens => Lenz, Store}

final case class KPair[F[_[_]], G[_[_]], A[_]](_1: F[A], _2: G[A])

object KPair {

  sealed trait Builder { // linter:ignore UnextendedSealedTrait
    type Out[A[_]]
  }

  type :**:[F[_[_]], G[_[_]]] = Builder {
    type Out[A[_]] = KPair[F, G, A]
  }

  type :*:[F[_[_]], B <: Builder] = Builder {
    type Out[A[_]] = KPair[F, B#Out, A]
  }

  implicit def fstLenz[F[_[_]], G[_[_]], A[_]]: Lenz[KPair[F, G, A], F[A]] =
    Lenz[KPair[F, G, A], F[A]](fga => Store(fa => KPair(fa, fga._2), fga._1))

  implicit def sndLenz[F[_[_]], G[_[_]], A[_]]: Lenz[KPair[F, G, A], G[A]] =
    Lenz[KPair[F, G, A], G[A]](fga => Store(ga => KPair(fga._1, ga), fga._2))

  implicit def fstRecLenz[F[_[_]], G[_[_]], A[_], B](implicit fab: Lenz[F[A], B]): Lenz[KPair[F, G, A], B] =
    fstLenz[F, G, A].andThen(fab)

  implicit def sndRecLenz[F[_[_]], G[_[_]], A[_], B](implicit gab: Lenz[G[A], B]): Lenz[KPair[F, G, A], B] =
    sndLenz[F, G, A].andThen(gab)

  implicit def fstLenzK[F[_[_]], G[_[_]]]: `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], F[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], F[A]]]] {
      def compute[A[_]]: Lenz[KPair[F, G, A], F[A]] =
        Lenz[KPair[F, G, A], F[A]](fga => Store(fa => KPair(fa, fga._2), fga._1))
    }

  implicit def sndLenzK[F[_[_]], G[_[_]]]: `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], G[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], G[A]]]] {
      def compute[A[_]]: Lenz[KPair[F, G, A], G[A]] =
        Lenz[KPair[F, G, A], G[A]](fga => Store(ga => KPair(fga._1, ga), fga._2))
    }

  implicit def fstRecLenzK[F[_[_]], G[_[_]], H[_[_]]](implicit fh: `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[F[A], H[A]]]]): `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], H[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], H[A]]]] {
      def compute[A[_]]: Lenz[KPair[F, G, A], H[A]] =
        Lenz[KPair[F, G, A], H[A]](fga => Store(ha => KPair(fh[A].set(fga._1, ha), fga._2), fh[A].get(fga._1)))
    }

  implicit def sndRecLenzK[F[_[_]], G[_[_]], H[_[_]]](implicit gh: `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[G[A], H[A]]]]): `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], H[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lenz[KPair[F, G, A], H[A]]]] {
      def compute[A[_]]: Lenz[KPair[F, G, A], H[A]] =
        Lenz[KPair[F, G, A], H[A]](fga => Store(ha => KPair(fga._1, gh[A].set(fga._2, ha)), gh[A].get(fga._2)))
    }

  implicit class AnyOps[G[_[_]], A[_]](val g: G[A]) extends AnyVal {
    def :**:[F[_[_]]](f: F[A]): KPair[F, G, A] = KPair(f, g)
  }

  implicit class KPairOps[G[_[_]], H[_[_]], A[_]](val p: KPair[G, H, A]) extends AnyVal {
    def :*:[F[_[_]]](f: F[A]): KPair[F, (G :**: H)#Out, A] = KPair(f, p)
  }
}
