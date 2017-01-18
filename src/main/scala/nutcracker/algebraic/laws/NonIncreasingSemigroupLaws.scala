package nutcracker.algebraic.laws

import nutcracker.algebraic.NonIncreasingSemigroup
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet

case class NonIncreasingSemigroupLaws[A: Arbitrary](S: NonIncreasingSemigroup[A]) extends LawSet("NonIncreasingSemigroup") {
  implicit val nonIncreasingSemigroup = S

  override val bases = Seq("semigroupWithOrder" -> SemigroupWithOrderLaws(S))
  override val props = {
    val laws = NonIncreasingSemigroup.laws[A]
    Seq(
      "leftAbsorption" -> forAll(laws.leftAbsorption _),
      "rightAbsorption" -> forAll(laws.rightAbsorption _)
    )
  }
}