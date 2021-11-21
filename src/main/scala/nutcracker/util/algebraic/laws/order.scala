package nutcracker.util.algebraic.laws

import scalaprops.{Check, Gen, Properties}
import scalaprops.Property.forAll
import scalaz.Order
import scalaz.syntax.order.ToOrderOps

object order {

  def reflexivity[A](implicit ev: Order[A], GA: Gen[A]) =
    forAll((a: A) => a lte a)

  def antisymmetry[A](implicit ev: Order[A], GA: Gen[A]) =
    forAll((a: A, b: A) => (a cmp b) == (b cmp a).complement)

  def transitivity[A](implicit ev: Order[A], GA: Gen[A]) =
    forAll((a: A, b: A, c: A) => {
      if(a lte b)
        !(b lte c) || (a lte c)
      else // a > b
        !(b gte c) || (a gte c)
    })

  def laws[A](implicit ev: Order[A], GA: Gen[A]) =
    Properties.fromChecks("Order")(
      "reflexivity" -> Check(reflexivity[A]),
      "antisymmetry" -> Check(antisymmetry[A]),
      "transitivity" -> Check(transitivity[A])
    )

  def all[A](implicit ev: Order[A], GA: Gen[A]) =
    Properties.fromProps("Order all", laws[A])
}