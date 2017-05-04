package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.NonIncreasingSemigroup
import scalaprops.{Check, Gen, Properties}
import scalaprops.Property.forAll
import scalaz.std.string._

object nonIncreasingSemigroup {

  def laws[A](implicit ev: NonIncreasingSemigroup[A], GA: Gen[A]) = {
    val laws = NonIncreasingSemigroup.laws[A]
    Properties.fromChecks("NonIncreasingSemigroup")(
      "leftAbsorption" -> Check(forAll(laws.leftAbsorption _)),
      "rightAbsorption" -> Check(forAll(laws.rightAbsorption _))
    )
  }

  def all[A](implicit ev: NonIncreasingSemigroup[A], GA: Gen[A]) =
    Properties.fromProps("NonIncreasingSemigroup all", laws[A], semigroupWithOrder.all[A])
}