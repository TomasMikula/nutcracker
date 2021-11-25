package nutcracker.util

import nutcracker.util.typealigned.BalancedComposer.{Post, Pre}

package object typealigned {

  /** Type-aligned pair. */
  type APair[F[_], G[_]] = BoundedAPair[Any, F, G]

  type Op[F[_, _], A, B] = F[B, A]

  /** Contravariant functor is a (covariant) functor in the opposite category. */
  type ContravariantLike[F[_], ->[_, _]] = FunctorLike[F, Op[->, *, *]]

  /**
   * Binary counter-like accumulator for type-aligned binary type constructors,
   * with the most significant bit on the left and addition of new elements (i.e. "increment") from the right.
   */
  type BalancedPostComposer[F[_, _], A, B] = BalancedComposer[Op[F, *, *], B, A, Post]

  /**
   * Binary counter-like accumulator for type-aligned binary type constructors,
   * with the most significant bit on the right and addition of new elements (i.e. "increment") from the left.
   */
  type BalancedPreComposer[F[_, _], A, B] = BalancedComposer[F, A, B, Pre]
}
