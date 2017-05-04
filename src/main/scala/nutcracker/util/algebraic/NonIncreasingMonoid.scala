package nutcracker.util.algebraic

import scalaz.Monoid

/** Monoid with a property that the result of the monoid operation
  * is less than or equal to any of the operands:
  *
  *   ∀ a,b:
  *     a⊕b ≤ a
  *     a⊕b ≤ b
  *
  * This property implies that zero is the maximum element:
  *
  *   ∀a: a ≤ 0
  */
trait NonIncreasingMonoid[A] extends NonIncreasingSemigroup[A] with Monoid[A]

object NonIncreasingMonoid {
  def apply[A](implicit A: NonIncreasingMonoid[A]): NonIncreasingMonoid[A] = A
}