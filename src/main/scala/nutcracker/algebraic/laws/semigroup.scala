package nutcracker.algebraic.laws

import scalaprops.{Check, Gen, Properties}
import scalaprops.Property.forAll
import scalaz.{Equal, Semigroup}
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.semigroup.ToSemigroupOps

object semigroup {

  def associativity[A](implicit A: Semigroup[A], GA: Gen[A], EA: Equal[A]) =
    forAll((a: A, b: A, c: A) =>
      ((a |+| b) |+| c) === (a |+| (b |+| c))
    )

  def laws[A](implicit A: Semigroup[A], GA: Gen[A], EA: Equal[A]) =
    Properties.fromChecks("Semigroup")(
      "associativity" -> Check(associativity[A])
    )

  def all[A](implicit A: Semigroup[A], GA: Gen[A], EA: Equal[A]) =
    Properties.fromProps("Semigroup all", laws[A])

}