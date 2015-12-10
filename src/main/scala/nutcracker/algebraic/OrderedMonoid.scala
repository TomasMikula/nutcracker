package nutcracker.algebraic

import scalaz.{Monoid, Ordering}
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
trait OrderedMonoid[A] extends OrderedSemigroup[A] with Monoid[A] {

}

object OrderedMonoid {
  def apply[A](implicit A: OrderedMonoid[A]): OrderedMonoid[A] = A

  case class Laws[A: Arbitrary](M: OrderedMonoid[A]) extends LawSet("OrderedMonoid") {

    override val bases = Seq(
      "orderedSemigroup" -> OrderedSemigroup.Laws(M),
      "monoid" -> MissingLaws.MonoidLaws(M))

    override def props = Seq()
  }
}