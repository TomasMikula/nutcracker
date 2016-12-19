package nutcracker.util.typealigned

import scala.language.higherKinds

/**
 * Type-aligned pair. Isomorphic to
 *
 * {{{
 * (F[A], G[A]) forSome { type A }
 * }}}
 *
 * The "pivot" type `A` is intentionally "hidden" as a type member
 * (as opposed to being a type parameter), so that pairs that differ
 * only in the pivot are considered to be of the same type and thus
 * can be used as arguments to tail-optimized recursive calls.
 */
sealed abstract class APair[F[_], G[_]] {
  type A
  val _1: F[A]
  val _2: G[A]
}

object APair {
  def apply[F[_], G[_], A0](fa: F[A0], ga: G[A0]): APair[F, G] =
    new APair[F, G] { type A = A0; val _1 = fa; val _2 = ga }

  def unapply[F[_], G[_]](p: APair[F, G]): Option[(F[p.A], G[p.A])] =
    Some((p._1, p._2))

  /** Defer specifying `A`, so that it could possibly be inferred. */
  def of[F[_], G[_]]: Builder[F, G] = new Builder[F, G]

  class Builder[F[_], G[_]] {
    def apply[A](fa: F[A], ga: G[A]): APair[F, G] =
      APair[F, G, A](fa, ga)
  }
}
