package nutcracker.util.algebraic

import scalaz.{Order, Semigroup}

/**
  * An auxiliary trait that extends both Semigroup and Order, but does not
  * add any new laws.
  */
trait SemigroupWithOrder[A] extends Semigroup[A] with Order[A]

object SemigroupWithOrder {
  def apply[A](implicit A: SemigroupWithOrder[A]): SemigroupWithOrder[A] = A
}