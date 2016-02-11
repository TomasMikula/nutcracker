package nutcracker.algebraic

import org.scalacheck.Arbitrary
import principled.LawSet

/** Ordered monoid with an extra property
  *
  *   ∀ a,b:
  *     a ≤ a⊕b
  *     b ≤ a⊕b
  *
  * This property implies that zero is the minimum element:
  *
  *   ∀a: 0 ≤ a
  */
trait NonDecreasingMonoid[A] extends NonDecreasingSemigroup[A] with OrderPreservingMonoid[A] {

}

object NonDecreasingMonoid {
  def apply[A](implicit A: NonDecreasingMonoid[A]): NonDecreasingMonoid[A] = A

  case class Laws[A: Arbitrary](M: NonDecreasingMonoid[A]) extends LawSet("NonDecreasingMonoid") {

    override val bases = Seq(
      "nonDecreasingSemigroup" -> NonDecreasingSemigroup.Laws(M),
      "orderPreservingMonoid" -> OrderPreservingMonoid.Laws(M))

    override def props = Seq()
  }
}