package nutcracker.algebraic

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet
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

  case class Laws[A: Arbitrary](S: NonDecreasingSemigroup[A]) extends LawSet("NonDecreasingSemigroup") {
    implicit val nonDecreasingSemigroup = S

    override val bases = Seq("semigroupWithOrder" -> SemigroupWithOrder.Laws(S))
    override val props = Seq(
      "leftAbsorption" -> forAll((a: A, b: A) => a lte (a |+| b)),
      "rightAbsorption" -> forAll((a: A, b: A) => b lte (a |+| b)))
  }
}