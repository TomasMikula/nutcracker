package nutcracker.algebraic

import org.scalacheck.Arbitrary
import principled.LawSet

/** Ordered monoid with an extra property
  *
  *   ∀ a,b:
  *     a⊕b ≤ a
  *     a⊕b ≤ b
  *
  * This property implies that zero is the maximum element:
  *
  *   ∀a: a ≤ 0
  */
trait NonIncreasingMonoid[A] extends NonIncreasingSemigroup[A] with OrderPreservingMonoid[A] {

}

object NonIncreasingMonoid {
  def apply[A](implicit A: NonIncreasingMonoid[A]): NonIncreasingMonoid[A] = A

  case class Laws[A: Arbitrary](M: NonIncreasingMonoid[A]) extends LawSet("NonIncreasingMonoid") {

    override val bases = Seq(
      "nonIncreasingSemigroup" -> NonIncreasingSemigroup.Laws(M),
      "orderPreservingMonoid" -> OrderPreservingMonoid.Laws(M))

    override def props = Seq()
  }
}