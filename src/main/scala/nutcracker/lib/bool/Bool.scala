package nutcracker.lib.bool

import nutcracker.{Final, RelativelyComplementedDom, SplittableDomWithBottom, UpdateResult}

/** A domain for boolean values. It is a boolean algebra, with interpretation
  * of operations that is dual to the standard interpretation in a boolean
  * algebra: contradiction is at the top, logical _and_ is join, and logical
  * _or_ is meet.
  */
final case class Bool private(intValue: Int) extends AnyVal

object Bool {
  final case class Join(value: Bool) // extends AnyVal // value class may not wrap another user-defined value class
  type Update = Join

  type BoolDom = RelativelyComplementedDom[Bool] with SplittableDomWithBottom.Aux[Bool, Update, Unit]

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
      type Update = Bool.Update
      type Delta = Unit

      import nutcracker.Splittable._
      override def assess(d: Bool): Status[Update] = d match {
        case Anything => Unrefined(() => Some(List(Join(MustBeTrue), Join(MustBeFalse))))
        case Contradiction => Failed
        case _ => Refined
      }
      override def update[B <: Bool](d: B, u: Update): UpdateResult[Bool, IDelta, B] = {
        val res = Bool(d.intValue &  u.value.intValue)
        if(res == d) UpdateResult() else UpdateResult(res, ())
      }
      override def toJoinUpdate(d: Bool): Update = Join(d)
      override def toComplementUpdate(d: Bool): Update = Join(Bool(Anything.intValue & ~d.intValue))
      override def ljoin[B <: Bool](d1: B, d2: Bool): UpdateResult[Bool, IDelta, B] = update(d1, Join(d2))
      override def appendDeltas(d1: Delta, d2: Delta): Delta = ()
      override def bottom: Bool = Anything
    }
}
