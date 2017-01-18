package nutcracker.algebraic.laws

import nutcracker.algebraic.NonIncreasingMonoid
import org.scalacheck.Arbitrary
import principled.LawSet

case class NonIncreasingMonoidLaws[A: Arbitrary](M: NonIncreasingMonoid[A]) extends LawSet("NonIncreasingMonoid") {

  override val bases = Seq(
    "nonIncreasingSemigroup" -> NonIncreasingSemigroupLaws(M),
    "monoid" -> MonoidLaws(M))

  override def props = Seq()
}