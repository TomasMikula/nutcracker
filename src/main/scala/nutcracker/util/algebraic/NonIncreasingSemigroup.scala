package nutcracker.util.algebraic

import scalaz.syntax.order.ToOrderOps
import scalaz.syntax.semigroup.ToSemigroupOps

/** Semigroup with a property that the result of the semigroup operation
  * is less than or equal to any of the operands:
  *
  *   ∀ a,b:
  *     a⊕b ≤ a
  *     a⊕b ≤ b
  */
trait NonIncreasingSemigroup[A] extends SemigroupWithOrder[A]

object NonIncreasingSemigroup {
  def apply[A](implicit A: NonIncreasingSemigroup[A]): NonIncreasingSemigroup[A] = A

  trait Laws[A] {
    implicit def A: NonIncreasingSemigroup[A]

    def leftAbsorption(a: A, b: A): Boolean = (a |+| b) lte a
    def rightAbsorption(a: A, b: A): Boolean = (a |+| b) lte b
  }

  def laws[A](implicit ev: NonIncreasingSemigroup[A]): Laws[A] = new Laws[A] {
    override val A = ev
  }
}