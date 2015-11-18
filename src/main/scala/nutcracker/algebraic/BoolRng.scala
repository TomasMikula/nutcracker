package nutcracker.algebraic

import scala.language.implicitConversions

import algebra.lattice.{BoundedJoinSemilattice, MeetSemilattice}
import simulacrum.typeclass

/** A Rng with idempotent multiplication, i.e.
  *
  *   a ∧ a = a
  *
  * This property also implies commutativity.
  *
  * Equivalently, BoolRng is a Boolean algebra (possibly) without the top
  * element and (absolute) complement, but with relative complement.
  */
@typeclass(excludeParents = List("MeetSemilattice", "BoundedJoinSemilattice"))
trait BoolRng[A] extends Any with CommutativeRng[A] with MeetSemilattice[A] with BoundedJoinSemilattice[A] {
  def zero: A
  @op("∧") def and(a: A, b: A): A
  @op("⊕") def xor(a: A, b: A): A
  @op("∨") def or(a: A, b: A): A
  @op("\\") def setMinus(a: A, b: A): A

  override def isZero(a: A)(implicit ev: algebra.Eq[A]): Boolean = ev.eqv(a, zero)

  /** Every element of a Boolean rng is its own additive inverse, i.e. a ⊕ a = 0. */
  def negate(a: A): A = a

  // aliases for zero
  def bottom: A = zero
  def ⊥ : A = zero

  // aliases for and
  def times(a: A, b: A): A = and(a, b)
  def meet(a: A, b: A): A = and(a, b)

  // aliases for xor
  def plus(a: A, b: A): A = xor(a, b)
  def symmDiff(a: A, b: A): A = xor(a, b)

  // aliases for or
  def join(a: A, b: A): A = or(a, b)

  // aliases for setMinus
  def relativeComplement(a: A, b: A): A = setMinus(a, b)
}