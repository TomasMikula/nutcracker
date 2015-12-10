package nutcracker.algebraic

import scalaz.Semigroup
import scalaz.Order
import principled.LawSet

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll, BooleanOperators}
import MissingLaws.{SemigroupLaws, OrderLaws}

import nutcracker.algebraic.MissingLaws.OrderLaws;
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
trait OrderedSemigroup[A] extends Semigroup[A] with Order[A] {

}

object OrderedSemigroup {
  def apply[A](implicit A: OrderedSemigroup[A]): OrderedSemigroup[A] = A

  case class Laws[A: Arbitrary](S: OrderedSemigroup[A]) extends LawSet("OrderedSemigroup") {
    implicit def orderedSemigroup = S

    override val bases = Seq(
      "semigroup" -> SemigroupLaws[A](S),
      "order" -> OrderLaws[A](S))

    override val props = Seq(
      "leftCompatibility" -> forAll((a: A, b: A, c: A) =>
        (a cmp b) == ((c |+| a) cmp (c |+| b))),
      "rightCompatibility" -> forAll((a: A, b: A, c: A) =>
        (a cmp b) == ((a |+| c) cmp (b |+| c))))
  }
}