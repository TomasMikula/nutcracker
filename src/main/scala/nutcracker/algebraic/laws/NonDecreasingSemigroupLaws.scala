package nutcracker.algebraic.laws

import nutcracker.algebraic.NonDecreasingSemigroup
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet

case class NonDecreasingSemigroupLaws[A: Arbitrary](S: NonDecreasingSemigroup[A]) extends LawSet("NonDecreasingSemigroup") {
  implicit val nonDecreasingSemigroup = S

  override val bases = Seq("semigroupWithOrder" -> SemigroupWithOrderLaws(S))
  override val props = {
    val laws = NonDecreasingSemigroup.laws[A]
    Seq(
      "leftAbsorption" -> forAll(laws.leftAbsorption _),
      "rightAbsorption" -> forAll(laws.rightAbsorption _)
    )
  }
}