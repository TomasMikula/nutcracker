package nutcracker.lib.bool

import algebra.lattice.Bool
import nutcracker.Domain

sealed trait BoolDomain

object BoolDomain {

  case object Top extends BoolDomain
  case object Bottom extends BoolDomain
  case object MustBeTrue extends BoolDomain
  case object MustBeFalse extends BoolDomain

  implicit val boolDomain: Domain[Boolean, BoolDomain] with Bool[BoolDomain] =
    new Domain[Boolean, BoolDomain] with Bool[BoolDomain] {
      def zero: BoolDomain = Bottom
      def one: BoolDomain = Top
      def and(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Top, x) => x
        case (x, Top) => x
        case (x, y) if x == y => x
        case _ => Bottom
      }
      def refine(a: BoolDomain, b: BoolDomain): Option[BoolDomain] = (a, b) match {
        case (_, Top) => None
        case (Top, y) => Some(y)
        case (Bottom, _) => None
        case (x, y) => if(x == y) None else Some(Bottom)
      }
      def or(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Bottom, x) => x
        case (x, Bottom) => x
        case (x, y) if x == y => x
        case _ => Top
      }
      def complement(a: BoolDomain): BoolDomain = a match {
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
      override def isEmpty(d: BoolDomain): Boolean = d == Bottom
    }
}
