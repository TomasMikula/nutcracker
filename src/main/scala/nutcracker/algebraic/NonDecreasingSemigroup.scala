package nutcracker.algebraic

import scalaz.syntax.order.ToOrderOps
import scalaz.syntax.semigroup.ToSemigroupOps

/** Semigroup with a property that the result of the semigroup operation
  * is greater than or equal to any of the operands:
  *
  *   ∀ a,b:
  *     a ≤ a⊕b
  *     b ≤ a⊕b
  */
trait NonDecreasingSemigroup[A] extends SemigroupWithOrder[A]

object NonDecreasingSemigroup {
  def apply[A](implicit A: NonDecreasingSemigroup[A]): NonDecreasingSemigroup[A] = A

  trait Laws[A] {
    implicit def A: NonDecreasingSemigroup[A]

    def leftAbsorption(a: A, b: A): Boolean = a lte (a |+| b)
    def rightAbsorption(a: A, b: A): Boolean = b lte (a |+| b)
  }

  def laws[A](implicit ev: NonDecreasingSemigroup[A]): Laws[A] = new Laws[A] {
    override val A = ev
  }
}