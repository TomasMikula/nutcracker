package nutcracker.lib.bool

import algebra.lattice.Bool
import nutcracker.Dom.{CMUDom, Diff, Meet}
import nutcracker.{Dom, EmbedExtract}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

sealed trait BoolDomain

object BoolDomain {

  case object Top extends BoolDomain
  case object Bottom extends BoolDomain
  case object MustBeTrue extends BoolDomain
  case object MustBeFalse extends BoolDomain

  implicit val embedExtractInstance: EmbedExtract[Boolean, BoolDomain] = new EmbedExtract[Boolean, BoolDomain] {

    def embed(b: Boolean): BoolDomain =
      if(b) MustBeTrue
      else  MustBeFalse

    def extract(d: BoolDomain): Option[Boolean] = d match {
      case MustBeTrue => Some(true)
      case MustBeFalse => Some(false)
      case _ => None
    }
  }

  implicit val boolDomain: CMUDom[BoolDomain] with Bool[BoolDomain] =
    new CMUDom[BoolDomain] with Bool[BoolDomain] {
      override def zero: BoolDomain = Bottom
      override def one: BoolDomain = Top
      override def and(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Top, x) => x
        case (x, Top) => x
        case (x, y) if x == y => x
        case _ => Bottom
      }
      override def or(a: BoolDomain, b: BoolDomain): BoolDomain = (a, b) match {
        case (Bottom, x) => x
        case (x, Bottom) => x
        case (x, y) if x == y => x
        case _ => Top
      }
      override def complement(a: BoolDomain): BoolDomain = a match {
        case Top => Bottom
        case Bottom => Top
        case MustBeTrue => MustBeFalse
        case MustBeFalse => MustBeTrue
      }
      override def assess(d: BoolDomain): Dom.Status[Meet[BoolDomain] \/ Diff[BoolDomain]] = d match {
        case Top => Dom.Unrefined(() => Some(List(Meet(MustBeTrue).left, Meet(MustBeFalse).left)))
        case Bottom => Dom.Failed
        case _ => Dom.Refined
      }
      override def update(d: BoolDomain, u: Meet[BoolDomain] \/ Diff[BoolDomain]): Option[(BoolDomain, Unit)] = {
        val res = u match {
          case -\/(m) => and(d, m.value)
          case \/-(x) => and(d, complement(x.value))
        }
        if(res == d) None else Some((res, ()))
      }
      override def combineDiffs(d1: Unit, d2: Unit): Unit = ()
    }
}
