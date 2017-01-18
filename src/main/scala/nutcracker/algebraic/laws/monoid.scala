package nutcracker.algebraic.laws

import scalaprops.{Check, Gen, Properties}
import scalaprops.Property.forAll
import scalaz.{Equal, Monoid}
import scalaz.std.string._
import scalaz.syntax.equal._
import scalaz.syntax.semigroup.ToSemigroupOps

object monoid {

  def leftIdentity[A](implicit ev: Monoid[A], GA: Gen[A], EA: Equal[A]) =
    forAll((a: A) => (ev.zero |+| a) === a)

  def rightIdentity[A](implicit ev: Monoid[A], GA: Gen[A], EA: Equal[A]) =
    forAll((a: A) => (a |+| ev.zero) === a)

  def laws[A](implicit ev: Monoid[A], GA: Gen[A], EA: Equal[A]) =
    Properties.fromChecks("Monoid")(
      "leftIdentity" -> Check(leftIdentity[A]),
      "rightIdentity" -> Check(rightIdentity[A])
    )

  def all[A](implicit ev: Monoid[A], GA: Gen[A], EA: Equal[A]) =
    Properties.fromProps("Monoid all", laws[A], semigroup.all[A])
}