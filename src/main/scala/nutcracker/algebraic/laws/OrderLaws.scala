package nutcracker.algebraic.laws

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import principled.LawSet

import scalaz.Order
import scalaz.syntax.order.ToOrderOps

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