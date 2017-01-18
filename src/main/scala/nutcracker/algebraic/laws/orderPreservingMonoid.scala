package nutcracker.algebraic.laws

import nutcracker.algebraic.OrderPreservingMonoid
import scalaprops.{Gen, Properties}
import scalaz.std.string._

object orderPreservingMonoid {

  def all[A](implicit ev: OrderPreservingMonoid[A], GA: Gen[A]) =
    Properties.fromProps("OrderPreservingMonoid all", orderPreservingSemigroup.all[A], monoid.laws[A])
}