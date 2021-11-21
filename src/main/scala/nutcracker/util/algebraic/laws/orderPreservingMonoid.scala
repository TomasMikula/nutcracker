package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.OrderPreservingMonoid
import scalaprops.{Gen, Properties}

object orderPreservingMonoid {

  def all[A](implicit ev: OrderPreservingMonoid[A], GA: Gen[A]) =
    Properties.fromProps("OrderPreservingMonoid all", orderPreservingSemigroup.all[A], monoid.laws[A])
}