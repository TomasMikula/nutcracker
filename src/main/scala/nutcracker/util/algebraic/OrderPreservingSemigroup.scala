package nutcracker.util.algebraic

import scalaz.syntax.order.ToOrderOps
import scalaz.syntax.semigroup.ToSemigroupOps

/** Semigroup whose operation preserves the Order:
  *
  *   ∀ a,b,c:
  *     (a ≤ b) → (a⊕c ≤ b⊕c)
  *     (a ≤ b) → (c⊕a ≤ c⊕b)
  *
  * @see [[https://en.wikipedia.org/wiki/Ordered_semigroup]]
  */
trait OrderPreservingSemigroup[A] extends SemigroupWithOrder[A]

object OrderPreservingSemigroup {
  def apply[A](implicit A: OrderPreservingSemigroup[A]): OrderPreservingSemigroup[A] = A

  trait Laws[A] {
    implicit def A: OrderPreservingSemigroup[A]

    def leftCompatibility(a: A, b: A, c: A): Boolean =
      (a cmp b) == ((c |+| a) cmp (c |+| b))

    def rightCompatibility(a: A, b: A, c: A): Boolean =
      (a cmp b) == ((a |+| c) cmp (b |+| c))
  }

  def laws[A](implicit ev: OrderPreservingSemigroup[A]): Laws[A] = new Laws[A] {
    override val A = ev
  }
}