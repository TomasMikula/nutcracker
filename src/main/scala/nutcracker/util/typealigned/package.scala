package nutcracker.util

import scala.language.higherKinds


package object typealigned {

  type Op[F[_, _], A, B] = F[B, A]

  /** Contravariant functor is a (covariant) functor in the opposite category. */
  type ContravariantLike[F[_], ->[_, _]] = FunctorLike[F, Op[->, ?, ?]]

  /**
   * Binary counter-like accumulator for type-aligned binary type constructors,
   * with the most significant bit on the left and addition of new elements (i.e. "increment") from the right.
   */
  type LBinary[F[_, _], A, B] = Binary[Op[F, ?, ?], B, A]

  /**
   * Binary counter-like accumulator for type-aligned binary type constructors,
   * with the most significant bit on the right and addition of new elements (i.e. "increment") from the left.
   */
  type RBinary[F[_, _], A, B] = Binary[F, A, B]
}
