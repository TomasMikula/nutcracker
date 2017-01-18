package nutcracker.algebraic.laws

import nutcracker.algebraic.OrderPreservingSemigroup
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet

case class OrderPreservingSemigroupLaws[A: Arbitrary](S: OrderPreservingSemigroup[A]) extends LawSet("OrderPreservingSemigroup") {
  implicit def orderedSemigroup = S

  override val bases = Seq(
    "semigroupWithOrder" -> SemigroupWithOrderLaws(S)
  )

  override val props = {
    val laws = OrderPreservingSemigroup.laws[A]
    Seq(
      "leftCompatibility" -> forAll(laws.leftCompatibility _),
      "rightCompatibility" -> forAll(laws.rightCompatibility _)
    )
  }
}