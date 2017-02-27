package nutcracker.util

import scala.language.higherKinds
import scalaz.{Lens, Store}

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

  implicit def fstLens[F[_[_]], G[_[_]], A[_]]: Lens[KPair[F, G, A], F[A]] =
    Lens[KPair[F, G, A], F[A]](fga => Store(fa => KPair(fa, fga._2), fga._1))

  implicit def sndLens[F[_[_]], G[_[_]], A[_]]: Lens[KPair[F, G, A], G[A]] =
    Lens[KPair[F, G, A], G[A]](fga => Store(ga => KPair(fga._1, ga), fga._2))

  implicit def fstRecLens[F[_[_]], G[_[_]], A[_], B](implicit fab: Lens[F[A], B]): Lens[KPair[F, G, A], B] =
    fstLens[F, G, A].andThen(fab)

  implicit def sndRecLens[F[_[_]], G[_[_]], A[_], B](implicit gab: Lens[G[A], B]): Lens[KPair[F, G, A], B] =
    sndLens[F, G, A].andThen(gab)

  implicit def fstLensK[F[_[_]], G[_[_]]]: `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], F[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], F[A]]]] {
      def compute[A[_]]: Lens[KPair[F, G, A], F[A]] =
        Lens[KPair[F, G, A], F[A]](fga => Store(fa => KPair(fa, fga._2), fga._1))
    }

  implicit def sndLensK[F[_[_]], G[_[_]]]: `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], G[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], G[A]]]] {
      def compute[A[_]]: Lens[KPair[F, G, A], G[A]] =
        Lens[KPair[F, G, A], G[A]](fga => Store(ga => KPair(fga._1, ga), fga._2))
    }

  implicit def fstRecLensK[F[_[_]], G[_[_]], H[_[_]]](implicit fh: `Forall{(* -> *) -> *}`[λ[A[_] => Lens[F[A], H[A]]]]): `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], H[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], H[A]]]] {
      def compute[A[_]]: Lens[KPair[F, G, A], H[A]] =
        Lens[KPair[F, G, A], H[A]](fga => Store(ha => KPair(fh[A].set(fga._1, ha), fga._2), fh[A].get(fga._1)))
    }

  implicit def sndRecLensK[F[_[_]], G[_[_]], H[_[_]]](implicit gh: `Forall{(* -> *) -> *}`[λ[A[_] => Lens[G[A], H[A]]]]): `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], H[A]]]] =
    new `Forall{(* -> *) -> *}`[λ[A[_] => Lens[KPair[F, G, A], H[A]]]] {
      def compute[A[_]]: Lens[KPair[F, G, A], H[A]] =
        Lens[KPair[F, G, A], H[A]](fga => Store(ha => KPair(fga._1, gh[A].set(fga._2, ha)), gh[A].get(fga._2)))
    }

  implicit class AnyOps[G[_[_]], A[_]](val g: G[A]) extends AnyVal {
    def :*:[F[_[_]]](f: F[A]): KPair[F, G, A] = KPair(f, g)
  }
}
