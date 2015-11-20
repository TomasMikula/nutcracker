package nutcracker.theories.bool

import algebra.lattice.BoundedLattice
import nutcracker.Domain
import nutcracker.algebraic.BoolRing

sealed trait BoolDomain

object BoolDomain {

  case object Top extends BoolDomain
  case object Bottom extends BoolDomain
  case object MustBeTrue extends BoolDomain
  case object MustBeFalse extends BoolDomain

  implicit val boolDomain: Domain[Boolean, BoolDomain] with BoolRing[BoolDomain] with BoundedLattice[BoolDomain] =
    new Domain[Boolean, BoolDomain] with BoolRing[BoolDomain] with BoundedLattice[BoolDomain] {
      def zero: BoolDomain = Bottom
      def one: BoolDomain = Top
      def and(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Top, x) => x
        case (x, Top) => x
        case (x, y) if x == y => x
        case _ => Bottom
      }
      def or(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Bottom, x) => x
        case (x, Bottom) => x
        case (x, y) if x == y => x
        case _ => Top
      }
      def setMinus(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Bottom, x) => Bottom
        case (x, Bottom) => x
        case (Top, x) => complement(x)
        case (x, Top) => Bottom
        case (x, y) if x == y => Bottom
        case (x, y) if x != y => x
      }
      def xor(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Bottom, x) => x
        case (x, Bottom) => x
        case (Top, x) => complement(x)
        case (x, Top) => complement(x)
        case (x, y) if x == y => Bottom
        case (x, y) if x != y => Top
      }
      override def complement(a: BoolDomain): BoolDomain = a match {
        case Top => Bottom
        case Bottom => Top
        case MustBeTrue => MustBeFalse
        case MustBeFalse => MustBeTrue
      }
      def values(d: BoolDomain): Domain.Values[Boolean, BoolDomain] = d match {
        case Bottom => Domain.Empty()
        case MustBeTrue => Domain.Just(true)
        case MustBeFalse => Domain.Just(false)
        case Top => Domain.Many(Stream(List(MustBeTrue, MustBeFalse)))
      }
      def singleton(b: Boolean): BoolDomain =
        if(b) MustBeTrue
        else  MustBeFalse
      def sizeUpperBound(d: BoolDomain): Option[Long] = Some(d match {
        case Top => 2
        case Bottom => 0
        case _ => 1
      })
      def eqv(x: BoolDomain, y: BoolDomain): Boolean = x == y
      override def isEmpty(d: BoolDomain): Boolean = d == Bottom
    }
}
