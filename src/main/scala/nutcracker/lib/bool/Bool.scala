package nutcracker.lib.bool

import nutcracker.{Diff, Dom, DomWithBottom, Final, Join, JoinDom}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

/** A domain for boolean values. It is a boolean algebra, with interpretation
  * of operations that is dual to the standard interpretation in a boolean
  * algebra: contradiction is at the top, logical _and_ is join, and logical
  * _or_ is meet.
  */
final case class Bool private(intValue: Int) extends AnyVal

object Bool {
  type BoolDom = DomWithBottom.Aux[Bool, Join[Bool] \/ Diff[Bool], Unit]

  val Contradiction = Bool(0)
  val MustBeTrue = Bool(1)
  val MustBeFalse = Bool(2)
  val Anything = Bool(3)

  implicit val finalInstance: Final.Aux[Bool, Boolean] = new Final[Bool] {
    type Out = Boolean

    def extract(d: Bool): Option[Boolean] = d match {
      case MustBeTrue => Some(true)
      case MustBeFalse => Some(false)
      case _ => None
    }

    def embed(b: Boolean): Bool =
      if(b) MustBeTrue
      else  MustBeFalse
  }

  implicit val boolDomain: BoolDom =
    new JoinDom[Bool] with DomWithBottom[Bool] {
      type Update = Join[Bool] \/ Diff[Bool]
      type Delta = Unit

      override def assess(d: Bool): Dom.Status[Update] = d match {
        case Anything => Dom.Unrefined(() => Some(List(Join(MustBeTrue).left, Join(MustBeFalse).left)))
        case Contradiction => Dom.Failed
        case _ => Dom.Refined
      }
      override def update(d: Bool, u: Update): Option[(Bool, Delta)] = {
        val res = u match {
          case -\/(Join(x)) => Bool(d.intValue &  x.intValue)
          case \/-(Diff(x)) => Bool(d.intValue & ~x.intValue)
        }
        if(res == d) None else Some((res, ()))
      }
      override def ljoin(d1: Bool, d2: Bool): Option[(Bool, Delta)] = update(d1, -\/(Join(d2)))
      override def combineDeltas(d1: Delta, d2: Delta): Delta = ()
      override def bottom: Bool = Anything
    }
}
