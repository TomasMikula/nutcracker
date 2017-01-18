package nutcracker.algebraic.laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import principled.LawSet

import scalaz.Monoid
import scalaz.syntax.semigroup.ToSemigroupOps

case class MonoidLaws[A: Arbitrary](M: Monoid[A]) extends LawSet("Monoid") {
  implicit def monoid = M

  override val bases = Seq("semigroup" -> SemigroupLaws(M))

  override val props = Seq(
    "leftIdentity" -> forAll((a: A) =>
      (M.zero |+| a) == a),
    "rightIdentity" -> forAll((a: A) =>
      (a |+| M.zero) == a))
}
