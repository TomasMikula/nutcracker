package nutcracker.lib.bool

import nutcracker.{Diff, Dom, DomWithBottom, Final, Join, JoinDom}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

/** A domain for boolean values. It is a boolean algebra, with interpretation
  * of operations that is dual to the standard interpretation in a boolean
  * algebra: contradiction is at the top, logical _and_ is join, and logical
  * _or_ is meet.
  */
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
      type Update = Join[BoolDomain] \/ Diff[BoolDomain]
      type Delta = Unit

      override def assess(d: BoolDomain): Dom.Status[Update] = d match {
        case Anything => Dom.Unrefined(() => Some(List(Join(MustBeTrue).left, Join(MustBeFalse).left)))
        case Contradiction => Dom.Failed
        case _ => Dom.Refined
      }
      override def update(d: BoolDomain, u: Update): Option[(BoolDomain, Delta)] = {
        val res = u match {
          case -\/(Join(x)) => BoolDomain(d.intValue &  x.intValue)
          case \/-(Diff(x)) => BoolDomain(d.intValue & ~x.intValue)
        }
        if(res == d) None else Some((res, ()))
      }
      override def ljoin(d1: BoolDomain, d2: BoolDomain): Option[(BoolDomain, Delta)] = update(d1, -\/(Join(d2)))
      override def combineDeltas(d1: Delta, d2: Delta): Delta = ()
      override def bottom: BoolDomain = Anything
    }
}
