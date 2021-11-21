package nutcracker.util.algebraic.laws

import nutcracker.util.algebraic.SemigroupWithOrder
import scalaprops.{Gen, Properties}

object semigroupWithOrder {

  def all[A](implicit ev: SemigroupWithOrder[A], GA: Gen[A]) =
    Properties.fromProps("SemigroupWithOrder all", semigroup.all[A], order.all[A])

}