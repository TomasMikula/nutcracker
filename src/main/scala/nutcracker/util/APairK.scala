package nutcracker.util

import scalaz.{Lens, Store}

sealed abstract class APairK[F[_[_]], G[_[_]]] {
  type A[_]
  val _1: F[A]
  val _2: G[A]
}

object APairK {
  def apply[F[_[_]], G[_[_]], X[_]](fx: F[X], gx: G[X]): Pair[F, G, X] =
    Pair[F, G, X](fx, gx)

  case class Pair[F[_[_]], G[_[_]], X[_]](_1: F[X], _2: G[X]) extends APairK[F, G] {
    type A[P] = X[P]
  }

  sealed trait Builder {
    type Out[A[_]]

    def and[H[_[_]]]: Builder.Aux[Out, H] =
      Builder.pair[Out, H]
  }

  object Builder {
    type Aux[F[_[_]], G[_[_]]] = Builder { type Out[A[_]] = Pair[F, G, A] }

    def pair[F[_[_]], G[_[_]]]: Builder.Aux[F, G] =
      new Builder {
        type Out[A[_]] = Pair[F, G, A]
      }
  }

  object unit {
    def and[F[_[_]]]: Builder { type Out[A[_]] = F[A] } =
      new Builder { type Out[A[_]] = F[A] }
  }

  implicit def fstLens[F[_[_]], G[_[_]], A[_]]: Lens[Pair[F, G, A], F[A]] =
    Lens[Pair[F, G, A], F[A]](fga => Store(fa => Pair(fa, fga._2), fga._1))

  implicit def sndLens[F[_[_]], G[_[_]], A[_]]: Lens[Pair[F, G, A], G[A]] =
    Lens[Pair[F, G, A], G[A]](fga => Store(ga => Pair(fga._1, ga), fga._2))

  implicit def fstRecLens[F[_[_]], G[_[_]], A[_], B](implicit fab: Lens[F[A], B]): Lens[Pair[F, G, A], B] =
    fstLens[F, G, A].andThen(fab)

  implicit def sndRecLens[F[_[_]], G[_[_]], A[_], B](implicit gab: Lens[G[A], B]): Lens[Pair[F, G, A], B] =
    sndLens[F, G, A].andThen(gab)

  implicit class AnyOps[G[_[_]], A[_]](val g: G[A]) extends AnyVal {
    def :*:[F[_[_]]](f: F[A]): Pair[F, G, A] = Pair(f, g)
  }
}
