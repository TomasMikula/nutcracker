package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.NonDecreasingSemigroup
import scalaprops.{Check, Gen, Properties}
import scalaprops.Property.forAll
import scalaz.std.string._

object nonDecreasingSemigroup {

  def laws[A](implicit ev: NonDecreasingSemigroup[A], GA: Gen[A]) = {
    val laws = NonDecreasingSemigroup.laws[A]
    Properties.fromChecks("NonDecreasingSemigroup")(
      "leftAbsorption" -> Check(forAll(laws.leftAbsorption _)),
      "rightAbsorption" -> Check(forAll(laws.rightAbsorption _))
    )
  }

  def all[A](implicit ev: NonDecreasingSemigroup[A], GA: Gen[A]) =
    Properties.fromProps("NonDecreasingSemigroup all", laws[A], semigroupWithOrder.all[A])
}