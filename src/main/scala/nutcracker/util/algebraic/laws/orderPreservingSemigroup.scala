package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.OrderPreservingSemigroup
import scalaprops.{Check, Gen, Properties}
import scalaprops.Property.forAll

object orderPreservingSemigroup {

  def laws[A](implicit ev: OrderPreservingSemigroup[A], GA: Gen[A]) = {
    val laws = OrderPreservingSemigroup.laws[A]
    Properties.fromChecks("OrderPreservingSemigroup")(
      "leftCompatibility" -> Check(forAll(laws.leftCompatibility _)),
      "rightCompatibility" -> Check(forAll(laws.rightCompatibility _))
    )
  }

  def all[A](implicit ev: OrderPreservingSemigroup[A], GA: Gen[A]) =
    Properties.fromProps("OrderPreservingSemigroup all", laws[A], semigroupWithOrder.all[A])
}