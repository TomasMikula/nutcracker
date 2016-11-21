package nutcracker.util

import scala.language.higherKinds
import monocle.Lens
import scalaz.{Lens => Lenz, Store}

/** Non-empty heterogenous list all of whose elements are parameterized by the same type. */
sealed abstract class KList[A]

object KList {
  final case class Just[F[_], A](value: F[A]) extends KList[A]
  final case class Cons[F[_], G[_] <: KList[_], A](head: F[A], tail: G[A]) extends KList[A]

  sealed trait Builder {
    type Out[_] <: KList[_]
  }

  sealed trait :**:[F[_], G[_]] extends Builder {
    type Out[A] = Cons[F, Just[G, ?], A]
  }

  sealed trait :*:[F[_], B <: Builder] extends Builder {
    type Out[A] = Cons[F, B#Out, A]
  }

  implicit def justLens[F[_], A]: Lens[Just[F, A], F[A]] =
    Lens[Just[F, A], F[A]](_.value)(fa => la => Just(fa))

  implicit def headLens[F[_], G[_] <: KList[_], A]: Lens[Cons[F, G, A], F[A]] =
    Lens[Cons[F, G, A], F[A]](_.head)(fa => la => Cons(fa, la.tail))

  implicit def tailLens[F[_], G[_] <: KList[_], A]: Lens[Cons[F, G, A], G[A]] =
    Lens[Cons[F, G, A], G[A]](la => la.tail)(ga => la => Cons(la.head, ga))

  implicit def recTailLens[F[_], G[_] <: KList[_], A, B](implicit gab: Lens[G[A], B]): Lens[Cons[F, G, A], B] =
    Lens[Cons[F, G, A], B](la => gab.get(la.tail))(b => la => Cons(la.head, gab.set(b)(la.tail)))

  implicit def justLenz[F[_], A]: Lenz[Just[F, A], F[A]] =
    Lenz[Just[F, A], F[A]](la => Store(fa => Just(fa), la.value))

  implicit def headLenz[F[_], G[_] <: KList[_], A]: Lenz[Cons[F, G, A], F[A]] =
    Lenz[Cons[F, G, A], F[A]](la => Store(fa => Cons(fa, la.tail), la.head))

  implicit def tailLenz[F[_], G[_] <: KList[_], A]: Lenz[Cons[F, G, A], G[A]] =
    Lenz[Cons[F, G, A], G[A]](la => Store(ga => Cons(la.head, ga), la.tail))

  implicit def recTailLenz[F[_], G[_] <: KList[_], A, B](implicit gab: Lenz[G[A], B]): Lenz[Cons[F, G, A], B] =
    Lenz[Cons[F, G, A], B](la => Store(b => Cons(la.head, gab.set(la.tail, b)), gab.get(la.tail)))

  implicit def justLenzK[F[_]]: `Forall{* -> *}`[λ[A => Lenz[Just[F, A], F[A]]]] =
    new `Forall{* -> *}`[λ[A => Lenz[Just[F, A], F[A]]]] {
      def compute[A]: Lenz[Just[F, A], F[A]] =
        Lenz[Just[F, A], F[A]](la => Store(fa => Just(fa), la.value))
    }

  implicit def headLenzK[F[_], G[_] <: KList[_]]: `Forall{* -> *}`[λ[A => Lenz[Cons[F, G, A], F[A]]]] =
    new `Forall{* -> *}`[λ[A => Lenz[Cons[F, G, A], F[A]]]] {
      def compute[A]: Lenz[Cons[F, G, A], F[A]] =
        Lenz[Cons[F, G, A], F[A]](la => Store(fa => Cons(fa, la.tail), la.head))
    }

  implicit def tailLenzK[F[_], G[_] <: KList[_]]: `Forall{* -> *}`[λ[A => Lenz[Cons[F, G, A], G[A]]]] =
    new `Forall{* -> *}`[λ[A => Lenz[Cons[F, G, A], G[A]]]] {
      def compute[A]: Lenz[Cons[F, G, A], G[A]] =
        Lenz[Cons[F, G, A], G[A]](la => Store(ga => Cons(la.head, ga), la.tail))
    }

  implicit def recTailLenzK[F[_], G[_] <: KList[_], H[_]](implicit gh: `Forall{* -> *}`[λ[A => Lenz[G[A], H[A]]]]): `Forall{* -> *}`[λ[A => Lenz[Cons[F, G, A], H[A]]]] =
    new `Forall{* -> *}`[λ[A => Lenz[Cons[F, G, A], H[A]]]] {
      def compute[A]: Lenz[Cons[F, G, A], H[A]] =
        Lenz[Cons[F, G, A], H[A]](la => Store(ha => Cons(la.head, gh[A].set(la.tail, ha)), gh[A].get(la.tail)))
    }

  implicit class AnyOps[G[_], A](val g: G[A]) extends AnyVal {
    def :**:[F[_]](f: F[A]): Cons[F, Just[G, ?], A] = Cons(f, Just(g))
  }

  implicit class KListOps[L[_] <: KList[_], A](val l: L[A]) extends AnyVal {
    def :*:[F[_]](f: F[A]): Cons[F, L, A] = Cons(f, l)
  }
}
