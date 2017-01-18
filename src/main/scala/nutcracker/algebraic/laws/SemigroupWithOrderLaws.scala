package nutcracker.algebraic.laws

import nutcracker.algebraic.SemigroupWithOrder
import org.scalacheck.Arbitrary
import principled.LawSet

case class SemigroupWithOrderLaws[A: Arbitrary](S: SemigroupWithOrder[A]) extends LawSet("SemigroupWithOrder") {
  implicit def semigroupWithOrder = S

  override val bases = Seq(
    "semigroup" -> SemigroupLaws[A](S),
    "order" -> OrderLaws[A](S))

  override def props = Seq()
}