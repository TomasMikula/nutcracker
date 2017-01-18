package nutcracker.algebraic.laws

import nutcracker.algebraic.SemigroupWithOrder
import scalaprops.{Gen, Properties}
import scalaz.std.string._

object semigroupWithOrder {

  def all[A](implicit ev: SemigroupWithOrder[A], GA: Gen[A]) =
    Properties.fromProps("SemigroupWithOrder all", semigroup.all[A], order.all[A])

}