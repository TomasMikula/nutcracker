package nutcracker.algebraic

import principled.LawSet
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import scalaz.syntax.order.ToOrderOps
import scalaz.syntax.semigroup.ToSemigroupOps

/** Semigroup whose operation preserves the Order:
  *
  *   ∀ a,b,c:
  *     (a ≤ b) → (a⊕c ≤ b⊕c)
  *     (a ≤ b) → (c⊕a ≤ c⊕b)
  *
  * @see [[https://en.wikipedia.org/wiki/Ordered_semigroup]]
  */
trait OrderPreservingSemigroup[A] extends SemigroupWithOrder[A]

object OrderPreservingSemigroup {
  def apply[A](implicit A: OrderPreservingSemigroup[A]): OrderPreservingSemigroup[A] = A

  case class Laws[A: Arbitrary](S: OrderPreservingSemigroup[A]) extends LawSet("OrderPreservingSemigroup") {
    implicit def orderedSemigroup = S

    override val bases = Seq(
      "semigroupWithOrder" -> SemigroupWithOrder.Laws(S)
    )

    override val props = Seq(
      "leftCompatibility" -> forAll((a: A, b: A, c: A) =>
        (a cmp b) == ((c |+| a) cmp (c |+| b))),
      "rightCompatibility" -> forAll((a: A, b: A, c: A) =>
        (a cmp b) == ((a |+| c) cmp (b |+| c))))
  }
}