package nutcracker.algebraic

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet
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

  case class Laws[A: Arbitrary](S: NonIncreasingSemigroup[A]) extends LawSet("NonIncreasingSemigroup") {
    implicit val nonIncreasingSemigroup = S

    override val bases = Seq("semigroupWithOrder" -> SemigroupWithOrder.Laws(S))
    override val props = Seq(
      "leftAbsorption" -> forAll((a: A, b: A) => (a |+| b) lte a),
      "rightAbsorption" -> forAll((a: A, b: A) => (a |+| b) lte b))
  }
}