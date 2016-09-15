package nutcracker.algebraic

import scalaz.Monoid
import principled.LawSet
import org.scalacheck.Arbitrary

/** Monoid whose operation preserves the Order:
  *
  *   ∀ a,b,c:
  *     (a ≤ b) → (a⊕c ≤ b⊕c)
  *     (a ≤ b) → (c⊕a ≤ c⊕b)
  *
  * @see [[https://en.wikipedia.org/wiki/Ordered_semigroup]]
  */
trait OrderPreservingMonoid[A] extends OrderPreservingSemigroup[A] with Monoid[A] {

}

object OrderPreservingMonoid {
  def apply[A](implicit A: OrderPreservingMonoid[A]): OrderPreservingMonoid[A] = A

  case class Laws[A: Arbitrary](M: OrderPreservingMonoid[A]) extends LawSet("OrderPreservingMonoid") {

    override val bases = Seq(
      "orderPreservingSemigroup" -> OrderPreservingSemigroup.Laws(M),
      "monoid" -> MissingLaws.MonoidLaws(M))

    override def props = Seq()
  }
}