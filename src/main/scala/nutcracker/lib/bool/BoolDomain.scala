package nutcracker.lib.bool

import algebra.lattice.Bool
import nutcracker.{Diff, Dom, Embed, Extract, Meet}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

final case class BoolDomain private(intValue: Int) extends AnyVal

object BoolDomain {

  val Bottom = BoolDomain(0)
  val MustBeTrue = BoolDomain(1)
  val MustBeFalse = BoolDomain(2)
  val Top = BoolDomain(3)

  implicit val embedInstance: Embed[Boolean, BoolDomain] = new Embed[Boolean, BoolDomain] {
    def embed(b: Boolean): BoolDomain =
      if(b) MustBeTrue
      else  MustBeFalse
  }

  implicit val extractInstance: Extract[BoolDomain, Boolean] = new Extract[BoolDomain, Boolean] {
    def extract(d: BoolDomain): Option[Boolean] = d match {
      case MustBeTrue => Some(true)
      case MustBeFalse => Some(false)
      case _ => None
    }
  }

  implicit val boolDomain: BoolDom with Bool[BoolDomain] =
    new BoolDom with Bool[BoolDomain] {
      override def zero: BoolDomain = Bottom
      override def one: BoolDomain = Top
      override def and(a: BoolDomain, b: BoolDomain): BoolDomain =
        BoolDomain(a.intValue & b.intValue)
      override def or(a: BoolDomain, b: BoolDomain): BoolDomain =
        BoolDomain(a.intValue | b.intValue)
      override def complement(a: BoolDomain): BoolDomain =
        BoolDomain(~a.intValue & Top.intValue)
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
