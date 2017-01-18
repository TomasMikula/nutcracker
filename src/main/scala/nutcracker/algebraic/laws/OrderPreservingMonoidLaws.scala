package nutcracker.algebraic.laws

import nutcracker.algebraic.OrderPreservingMonoid
import org.scalacheck.Arbitrary
import principled.LawSet

case class OrderPreservingMonoidLaws[A: Arbitrary](M: OrderPreservingMonoid[A]) extends LawSet("OrderPreservingMonoid") {

  override val bases = Seq(
    "orderPreservingSemigroup" -> OrderPreservingSemigroupLaws(M),
    "monoid" -> MonoidLaws(M))

  override def props = Seq()
}