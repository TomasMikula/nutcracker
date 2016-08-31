package nutcracker.lib.bool

import nutcracker.{Diff, Dom, DomWithBottom, Embed, Final, Meet}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

final case class BoolDomain private(intValue: Int) extends AnyVal

object BoolDomain {

  val Contradiction = BoolDomain(0)
  val MustBeTrue = BoolDomain(1)
  val MustBeFalse = BoolDomain(2)
  val Anything = BoolDomain(3)

  implicit val embedInstance: Embed[Boolean, BoolDomain] = new Embed[Boolean, BoolDomain] {
    def embed(b: Boolean): BoolDomain =
      if(b) MustBeTrue
      else  MustBeFalse
  }

  implicit val finalInstance: Final.Aux[BoolDomain, Boolean] = new Final[BoolDomain] {
    type Out = Boolean

    def extract(d: BoolDomain): Option[Boolean] = d match {
      case MustBeTrue => Some(true)
      case MustBeFalse => Some(false)
      case _ => None
    }
  }

  implicit val boolDomain: BoolDom =
    new DomWithBottom[BoolDomain] {
      type Update = Meet[BoolDomain] \/ Diff[BoolDomain]
      type Delta = Unit

      override def assess(d: BoolDomain): Dom.Status[Meet[BoolDomain] \/ Diff[BoolDomain]] = d match {
        case Anything => Dom.Unrefined(() => Some(List(Meet(MustBeTrue).left, Meet(MustBeFalse).left)))
        case Contradiction => Dom.Failed
        case _ => Dom.Refined
      }
      override def update(d: BoolDomain, u: Meet[BoolDomain] \/ Diff[BoolDomain]): Option[(BoolDomain, Unit)] = {
        val res = u match {
          case -\/(Meet(x)) => BoolDomain(d.intValue &  x.intValue)
          case \/-(Diff(x)) => BoolDomain(d.intValue & ~x.intValue)
        }
        if(res == d) None else Some((res, ()))
      }
      override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
      override def bottom: BoolDomain = Anything
    }
}
