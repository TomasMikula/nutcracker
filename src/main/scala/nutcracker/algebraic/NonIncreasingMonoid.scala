package nutcracker.algebraic

import org.scalacheck.Arbitrary
import principled.LawSet
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

  case class Laws[A: Arbitrary](M: NonIncreasingMonoid[A]) extends LawSet("NonIncreasingMonoid") {

    override val bases = Seq(
      "nonIncreasingSemigroup" -> NonIncreasingSemigroup.Laws(M),
      "monoid" -> MissingLaws.MonoidLaws(M))

    override def props = Seq()
  }
}