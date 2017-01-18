package nutcracker.algebraic.laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet

import scalaz.Semigroup
import scalaz.syntax.semigroup.ToSemigroupOps

case class SemigroupLaws[A: Arbitrary](S: Semigroup[A]) extends LawSet("Semigroup") {
  implicit def semigroup = S

  override def bases = Seq()

  override val props = Seq(
    "associativity" -> forAll((a: A, b: A, c: A) =>
        ((a |+| b) |+| c) == (a |+| (b |+| c)))
  )
}