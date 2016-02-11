package nutcracker.algebraic

import scalaz.{Semigroup, Order}
import nutcracker.algebraic.MissingLaws.{OrderLaws, SemigroupLaws}
import org.scalacheck.Arbitrary
import principled.LawSet

/**
  * An auxiliary trait that extends both Semigroup and Order, but does not
  * add any new laws.
  */
trait SemigroupWithOrder[A] extends Semigroup[A] with Order[A]

object SemigroupWithOrder {
  def apply[A](implicit A: SemigroupWithOrder[A]): SemigroupWithOrder[A] = A

  case class Laws[A: Arbitrary](S: SemigroupWithOrder[A]) extends LawSet("SemigroupWithOrder") {
    implicit def semigroupWithOrder = S

    override val bases = Seq(
      "semigroup" -> SemigroupLaws[A](S),
      "order" -> OrderLaws[A](S))

    override def props = Seq()
  }
}