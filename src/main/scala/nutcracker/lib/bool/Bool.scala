package nutcracker.lib.bool

import nutcracker.{Diff, Final, Join, RelativelyComplementedDom, SplittableDomWithBottom, UpdateResult}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

/** A domain for boolean values. It is a boolean algebra, with interpretation
  * of operations that is dual to the standard interpretation in a boolean
  * algebra: contradiction is at the top, logical _and_ is join, and logical
  * _or_ is meet.
  */
final case class Bool private(intValue: Int) extends AnyVal

object Bool {
  type BoolDom = RelativelyComplementedDom[Bool] with SplittableDomWithBottom.Aux[Bool, Join[Bool] \/ Diff[Bool], Unit]

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
    new RelativelyComplementedDom[Bool] with SplittableDomWithBottom[Bool] {
      type Update = Join[Bool] \/ Diff[Bool]
      type Delta = Unit

      import nutcracker.Splittable._
      override def assess(d: Bool): Status[Update] = d match {
        case Anything => Unrefined(() => Some(List(Join(MustBeTrue).left, Join(MustBeFalse).left)))
        case Contradiction => Failed
        case _ => Refined
      }
      override def update[B <: Bool](d: B, u: Update): UpdateResult[Bool, IDelta, B] = {
        val res = u match {
          case -\/(Join(x)) => Bool(d.intValue &  x.intValue)
          case \/-(Diff(x)) => Bool(d.intValue & ~x.intValue)
        }
        if(res == d) UpdateResult() else UpdateResult(res, ())
      }
      override def toJoinUpdate(d: Bool): Update = -\/(Join(d))
      override def toComplementUpdate(d: Bool): Update = \/-(Diff(d))
      override def ljoin[B <: Bool](d1: B, d2: Bool): UpdateResult[Bool, IDelta, B] = update(d1, -\/(Join(d2)))
      override def appendDeltas(d1: Delta, d2: Delta): Delta = ()
      override def bottom: Bool = Anything
    }
}
