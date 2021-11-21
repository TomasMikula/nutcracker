package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.NonIncreasingMonoid
import scalaprops.{Gen, Properties}

object nonIncreasingMonoid {

  def all[A](implicit ev: NonIncreasingMonoid[A], GA: Gen[A]) =
    Properties.fromProps("NonIncreasingMonoid all", nonIncreasingSemigroup.all[A], monoid.laws[A])

}