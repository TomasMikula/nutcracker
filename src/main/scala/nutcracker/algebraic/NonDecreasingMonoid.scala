package nutcracker.algebraic

import scalaz.Monoid

/** Monoid with a property that the result of the monoid operation
  * is greater than or equal to any of the operands:
  *
  *   ∀ a,b:
  *     a ≤ a⊕b
  *     b ≤ a⊕b
  *
  * This property implies that zero is the minimum element:
  *
  *   ∀a: 0 ≤ a
  */
trait NonDecreasingMonoid[A] extends NonDecreasingSemigroup[A] with Monoid[A]

object NonDecreasingMonoid {
  def apply[A](implicit A: NonDecreasingMonoid[A]): NonDecreasingMonoid[A] = A
}