package nutcracker.data.bool

import nutcracker.{Final, RelativelyComplementedDom, SplittableDomWithBottom, TerminalDom, UpdateResult}

/** A domain for boolean values. It is a boolean algebra, with interpretation
  * of operations that is dual to the standard interpretation in a boolean
  * algebra: contradiction is at the top, logical _and_ is join, and logical
  * _or_ is meet.
  */
sealed abstract class Bool(val intValue: Int)

object Bool {
  final case class Join(value: Bool) // extends AnyVal // value class may not wrap another user-defined value class
  type Update = Join

  type BoolDom = RelativelyComplementedDom[Bool] with SplittableDomWithBottom.Aux[Bool, Update, Unit] with TerminalDom[Bool]

  case object Contradiction extends Bool(0)
  case object MustBeTrue extends Bool(1)
  case object MustBeFalse extends Bool(2)
  case object Anything extends Bool(3)

  private def apply(i: Int): Bool = i match {
    case 0 => Contradiction
    case 1 => MustBeTrue
    case 2 => MustBeFalse
    case 3 => Anything
    case _ => sys.error("Illegal argument: " + i)
  }

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
    new RelativelyComplementedDom[Bool] with SplittableDomWithBottom[Bool] with TerminalDom[Bool] {
      type Update = Bool.Update
      type Delta = Unit

      import nutcracker.Splittable._
      override def assess(d: Bool): Status[Update] = d match {
        case Anything => Unrefined(() => Some(List(Join(MustBeTrue), Join(MustBeFalse))))
        case Contradiction => Failed
        case _ => Refined
      }
      override def update(d: Bool, u: Update): UpdateResult[Bool, Delta] = {
        val res = Bool(d.intValue &  u.value.intValue)
        if(res == d) UpdateResult.unchanged else UpdateResult.updated(res, ())
      }
      override def toJoinUpdate(d: Bool): Update = Join(d)
      override def toComplementUpdate(d: Bool): Update = Join(Bool(Anything.intValue & ~d.intValue))
      override def ljoin(d1: Bool, d2: Bool): UpdateResult[Bool, Delta] = update(d1, Join(d2))
      override def appendDeltas(d1: Delta, d2: Delta): Delta = ()
      override def bottom: Bool = Anything
      override def terminate: Update = Join(Bool.Contradiction)
    }
}
