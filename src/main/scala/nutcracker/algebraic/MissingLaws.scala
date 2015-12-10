package nutcracker.algebraic

import scalaz.Semigroup
import principled.LawSet
import scalaz.Order
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Arbitrary
import scalaz.syntax.semigroup.ToSemigroupOps
import scalaz.syntax.order.ToOrderOps
import scalaz.Monoid

object MissingLaws {
  case class SemigroupLaws[A: Arbitrary](S: Semigroup[A]) extends LawSet("Semigroup") {
    implicit def semigroup = S

    override def bases = Seq()

    override val props = Seq(
      "associativity" -> forAll((a: A, b: A, c: A) =>
          ((a |+| b) |+| c) == (a |+| (b |+| c)))
    )
  }

  case class OrderLaws[A: Arbitrary](O: Order[A]) extends LawSet("Order") {
    implicit def order = O

    override def bases = Seq()

    override val props = Seq(
      "reflexivity" -> forAll((a: A) =>
        a lte a),
      "antisymmetry" -> forAll((a: A, b: A) =>
        (a cmp b) == (b cmp a).complement),
      "transitivity" -> forAll((a: A, b: A, c: A) => {
        if(a lte b)
          (b lte c) ==> (a lte c)
        else // a > b
          (b gte c) ==> (a gte c)
      }))
  }

  case class MonoidLaws[A: Arbitrary](M: Monoid[A]) extends LawSet("Monoid") {
    implicit def monoid = M

    override val bases = Seq("semigroup" -> SemigroupLaws(M))

    override val props = Seq(
      "leftIdentity" -> forAll((a: A) =>
        (M.zero |+| a) == a),
      "rightIdentity" -> forAll((a: A) =>
        (a |+| M.zero) == a))
  }
}