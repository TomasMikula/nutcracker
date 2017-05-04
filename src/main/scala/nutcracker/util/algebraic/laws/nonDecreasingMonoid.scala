package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.NonDecreasingMonoid
import scalaprops.{Gen, Properties}
import scalaz.std.string._

object nonDecreasingMonoid {

  def all[A](implicit ev: NonDecreasingMonoid[A], GA: Gen[A]) =
    Properties.fromProps("NonDecreasingMonoid all", nonDecreasingSemigroup.all[A], monoid.laws[A])

}