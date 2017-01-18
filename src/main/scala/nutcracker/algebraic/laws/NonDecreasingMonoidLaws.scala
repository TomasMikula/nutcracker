package nutcracker.algebraic.laws

import nutcracker.algebraic.NonDecreasingMonoid
import org.scalacheck.Arbitrary
import principled.LawSet

case class NonDecreasingMonoidLaws[A: Arbitrary](M: NonDecreasingMonoid[A]) extends LawSet("NonDecreasingMonoid") {

  override val bases = Seq(
    "nonDecreasingSemigroup" -> NonDecreasingSemigroupLaws(M),
    "monoid" -> MonoidLaws(M))

  override def props = Seq()
}