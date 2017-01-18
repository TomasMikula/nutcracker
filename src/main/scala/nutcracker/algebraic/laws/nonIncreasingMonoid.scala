package nutcracker.algebraic.laws

import nutcracker.algebraic.NonIncreasingMonoid
import scalaprops.{Gen, Properties}
import scalaz.std.string._

object nonIncreasingMonoid {

  def all[A](implicit ev: NonIncreasingMonoid[A], GA: Gen[A]) =
    Properties.fromProps("NonIncreasingMonoid all", nonIncreasingSemigroup.all[A], monoid.laws[A])

}