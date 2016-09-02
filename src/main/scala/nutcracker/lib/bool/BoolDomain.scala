package nutcracker.lib.bool

import nutcracker.{Diff, Dom, DomWithBottom, Final, JoinDom, Meet}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

final case class BoolDomain private(intValue: Int) extends AnyVal

object BoolDomain {

  val Contradiction = BoolDomain(0)
  val MustBeTrue = BoolDomain(1)
  val MustBeFalse = BoolDomain(2)
  val Anything = BoolDomain(3)

  implicit val finalInstance: Final.Aux[BoolDomain, Boolean] = new Final[BoolDomain] {
    type Out = Boolean

    def extract(d: BoolDomain): Option[Boolean] = d match {
      case MustBeTrue => Some(true)
      case MustBeFalse => Some(false)
      case _ => None
    }

    def embed(b: Boolean): BoolDomain =
      if(b) MustBeTrue
      else  MustBeFalse
  }

  implicit val boolDomain: BoolDom =
    new JoinDom[BoolDomain] with DomWithBottom[BoolDomain] {
      type Update = Meet[BoolDomain] \/ Diff[BoolDomain]
      type Delta = Unit

      override def assess(d: BoolDomain): Dom.Status[Update] = d match {
        case Anything => Dom.Unrefined(() => Some(List(Meet(MustBeTrue).left, Meet(MustBeFalse).left)))
        case Contradiction => Dom.Failed
        case _ => Dom.Refined
      }
      override def update(d: BoolDomain, u: Update): Option[(BoolDomain, Delta)] = {
        val res = u match {
          case -\/(Meet(x)) => BoolDomain(d.intValue &  x.intValue)
          case \/-(Diff(x)) => BoolDomain(d.intValue & ~x.intValue)
        }
        if(res == d) None else Some((res, ()))
      }
      override def ljoin(d1: BoolDomain, d2: BoolDomain): Option[(BoolDomain, Delta)] = update(d1, -\/(Meet(d2)))
      override def combineDeltas(d1: Delta, d2: Delta): Delta = ()
      override def bottom: BoolDomain = Anything
    }
}
