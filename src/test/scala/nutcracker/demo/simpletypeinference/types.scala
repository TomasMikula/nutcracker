package nutcracker.demo.simpletypeinference

import nutcracker.{IDom, IUpdateResult}
import nutcracker.util.Exists
import nutcracker.util.typealigned.{ANone, AOption, ASome}
import scalaz.Show
import scalaz.syntax.show._

object types {

  object Tag {
    sealed trait Var
    sealed trait Unit
    sealed trait Int
    sealed trait Str
    sealed trait App
    sealed trait Prod
    sealed trait Sum
    sealed trait Fix
    sealed trait Comp
    sealed trait Err
  }

  /** Represents a (non-parametric) type. */
  sealed trait TypeT[T[_[_]], I]

  case class TypeVar[T[_[_]]](aliases: Set[Object]) extends TypeT[T, Tag.Var]

  sealed trait NonVarTypeT[T[_[_]], I] extends TypeT[T, I]

  case class UnitTypeT[T[_[_]]]() extends NonVarTypeT[T, Tag.Unit]

  case class IntTypeT[T[_[_]]]() extends NonVarTypeT[T, Tag.Int]

  case class StringTypeT[T[_[_]]]() extends NonVarTypeT[T, Tag.Str]

  case class TypeAppT[T[_[_]]](
    f: T[Type1T[T, *]],
    x: T[TypeT[T, *]],
  ) extends NonVarTypeT[T, Tag.App]

  /** Recursive type. */
  case class FixTypeT[T[_[_]]](f: T[Type1T[T, *]]) extends NonVarTypeT[T, Tag.Fix]

  case class TypeErrorT[T[_[_]]](msg: String) extends NonVarTypeT[T, Tag.Err]

  def newTypeVar[T[_[_]]](): TypeT[T, Tag.Var] =
    new TypeVar[T](Set(new Object))

  object TypeT {
    /** Describes a change of `TypeT[T, I]` to `TypeT[T, J]`. */
    sealed trait ChangeT[T[_[_]], I, J] {
      import ChangeT._

      def andThen[K](that: ChangeT[T, J, K]): ChangeT[T, I, K] =
        (this, that) match {
          case (AddedVarAliases(newAliases1), AddedVarAliases(newAliases2)) => AddedVarAliases(newAliases1 union newAliases2)
          case (AddedVarAliases(_)          , SubstitutedForVar(newValue))  => SubstitutedForVar(newValue)
          case (AddedVarAliases(_)          , Failed(e))                    => Failed(e)
          case (SubstitutedForVar(_)        , Failed(e))                    => Failed(e)
        }
    }

    object ChangeT {
      case class AddedVarAliases[T[_[_]]](newAliases: Set[Object]) extends ChangeT[T, Tag.Var, Tag.Var]
      case class SubstitutedForVar[T[_[_]], J](newValue: NonVarTypeT[T, J]) extends ChangeT[T, Tag.Var, J]
      case class Failed[T[_[_]], I](newValue: TypeErrorT[T]) extends ChangeT[T, I, Tag.Err]
    }

    /** Record of updating `TypeT[T, I]` by unifying it with `TypeT[T, J]`,
      * which resulted in `TypeT[T, K]`.
      *
      * It is richer than [[Change]] and can be reduced to it via [[change]].
      */
    sealed trait UpdRes[T[_[_]], I, J, K] {
      import UpdRes._

      type Change[X, Y] = ChangeT[T, X, Y]

      def toUpdateResult: IUpdateResult[TypeT[T, *], Change, I, K] =
        this match {
          case UnifiedVars(newVal, newAliases) => IUpdateResult.updated(newVal, ChangeT.AddedVarAliases(newAliases))
          case SubstitutedForVar(newVal)       => IUpdateResult.updated(newVal, ChangeT.SubstitutedForVar(newVal))
          case AlreadySuperset(_)              => IUpdateResult.unchanged
          case AlreadySamePrimitive()          => IUpdateResult.unchanged
          case AlreadyRefined(_)               => IUpdateResult.unchanged
          case AlreadyTypeApp(_)               => IUpdateResult.unchanged
          case AlreadyFixType(_)               => IUpdateResult.unchanged
          case Failed(newVal)                  => IUpdateResult.updated(newVal, ChangeT.Failed(newVal))
          case AlreadyFailed(_)                => IUpdateResult.unchanged
        }
    }

    object UpdRes {
      case class UnifiedVars[T[_[_]]](
        newValue: TypeVar[T],
        newlyAddedAliases: Set[Object], // subset of newValue.aliases
      ) extends UpdRes[T, Tag.Var, Tag.Var, Tag.Var]

      case class AlreadySuperset[T[_[_]]](newValue: TypeVar[T]) extends UpdRes[T, Tag.Var, Tag.Var, Tag.Var]

      case class AlreadySamePrimitive[T[_[_]], I, J]() extends UpdRes[T, I, J, I]

      case class SubstitutedForVar[T[_[_]], J](newValue: NonVarTypeT[T, J]) extends UpdRes[T, Tag.Var, J, J]

      case class AlreadyRefined[T[_[_]], I](value: NonVarTypeT[T, I]) extends UpdRes[T, I, Tag.Var, I]

      case class AlreadyTypeApp[T[_[_]], F[_]](value: TypeAppT[T]) extends UpdRes[T, Tag.App, Tag.App, Tag.App]

      case class AlreadyFixType[T[_[_]]](f: FixTypeT[T]) extends UpdRes[T, Tag.Fix, Tag.Fix, Tag.Fix]

      case class Failed[T[_[_]], I, J](newValue: TypeErrorT[T]) extends UpdRes[T, I, J, Tag.Err]

      case class AlreadyFailed[T[_[_]], J](value: TypeErrorT[T]) extends UpdRes[T, Tag.Err, J, Tag.Err]
    }

    implicit def domType[T[_[_]]]
    : IDom.Aux2[
        TypeT[T, *],
        TypeT[T, *],
        UpdRes[T, *, *, ?],
        ChangeT[T, *, *],
      ] =
      new IDom[TypeT[T, *]] {
        override type IUpdate[J]       = TypeT[T, J]
        override type IUpdateRes[I, J] = UpdRes[T, I, J, ?]
        override type IDelta[I, K]     = ChangeT[T, I, K]

        override def iUpdate[I, J](
          t: TypeT[T, I],
          u: TypeT[T, J],
        ): UpdRes[T, I, J, ?] =
          (t, u) match {
            case (t @ TypeVar(aliases1), TypeVar(aliases2)) =>
              val newAliases = aliases2 diff aliases1
              if (newAliases.nonEmpty) {
                val newVal = TypeVar[T](aliases1 union newAliases)
                UpdRes.UnifiedVars(newVal, newAliases)
              } else {
                UpdRes.AlreadySuperset(t)
              }

            case (TypeVar(_), u: NonVarTypeT[T, J]) =>
              UpdRes.SubstitutedForVar(u)

            case (t: NonVarTypeT[T, I], TypeVar(_)) =>
              UpdRes.AlreadyRefined(t)

            case (t @ TypeErrorT(_), _) =>
              UpdRes.AlreadyFailed(t)

            case (_, TypeErrorT(msg)) =>
              UpdRes.Failed(TypeErrorT(msg))

            case (UnitTypeT(), UnitTypeT()) =>
              UpdRes.AlreadySamePrimitive()

            case (IntTypeT(), IntTypeT()) =>
              UpdRes.AlreadySamePrimitive()

            case (StringTypeT(), StringTypeT()) =>
              UpdRes.AlreadySamePrimitive()

            case (ta: TypeAppT[T], TypeAppT(g, y)) =>
              UpdRes.AlreadyTypeApp(ta)

            case (f @ FixTypeT(_), FixTypeT(g)) =>
              UpdRes.AlreadyFixType(f)

            case (t, u) =>
              UpdRes.Failed(TypeErrorT(s"Cannot unify ${t.show} with ${u.show}"))
          }

        override def composeDeltas[I1, I2, I3](ε: IDelta[I2, I3], δ: IDelta[I1, I2]): IDelta[I1, I3] =
          δ andThen ε

        override def toUpdateResult[I, J](r: IUpdateRes[I, J]): IUpdateResult[TypeT[T, *], IDelta, I, ?] =
          r.toUpdateResult

        override def iIsFailed[I](t: TypeT[T, I]): Boolean =
          t match {
            case TypeErrorT(_) => true
            case _                 => false
          }
      }

    implicit def show[T[_[_]], I]: Show[TypeT[T, I]] =
      Show.shows {
        case UnitTypeT()    => "Unit"
        case TypeAppT(f, x) => "<type application>"
        case FixTypeT(_)    => "<fixed point type>"
        case other => throw new NotImplementedError(s"$other not handled")
      }
  }

  /** Represents a type constructor taking 1 type parameter (`F[_]`). */
  sealed trait Type1T[T[_[_]], I]

  case class Type1Var[T[_[_]]](aliases: Set[Object]) extends Type1T[T, Tag.Var]

  sealed trait NonVarType1T[T[_[_]], I] extends Type1T[T, I]

  case class TypeApp1T[T[_[_]]](
    f: Type2T[?], // we happen to not need to wrap this one in `T`
    x: T[TypeT[T, *]],
  ) extends NonVarType1T[T, Tag.App]

  case class Type1ErrorT[T[_[_]]](msg: String) extends NonVarType1T[T, Tag.Err]

  case class Composed1T[T[_[_]]](
    f: T[Type1T[T, *]],
    g: T[Type1T[T, *]],
  ) extends NonVarType1T[T, Tag.Comp]

  def newType1Var[T[_[_]]](): Type1T[T, Tag.Var] =
    new Type1Var[T](Set(new Object))

  object Type1T {
    sealed trait ChangeT[T[_[_]], I, J]

    object ChangeT {
      case class UnifiedVars[T[_[_]]](newAliases: Set[Object]) extends ChangeT[T, Tag.Var, Tag.Var]
      case class SubstitutedForVar[T[_[_]], J](newValue: Type1T[T, J]) extends ChangeT[T, Tag.Var, J]
      case class Failed[T[_[_]], I](newValue: Type1ErrorT[T]) extends ChangeT[T, I, Tag.Err]
    }

    sealed trait UpdRes[T[_[_]], I, J, K] {
      import UpdRes._

      type Change[X, Y] = ChangeT[T, X, Y]

      def toUpdateResult: IUpdateResult[Type1T[T, *], ChangeT[T, *, *], I, K] =
        this match {
          case Unchanged()                       => IUpdateResult.unchanged
          case UnifiedVars(newValue, newAliases) => IUpdateResult.updated(newValue, ChangeT.UnifiedVars(newAliases))
          case SubstitutedForVar(newValue)       => IUpdateResult.updated(newValue, ChangeT.SubstitutedForVar(newValue))
          case AlreadyTypeApp(_)                 => IUpdateResult.unchanged
          case AlreadyComposed(_)                => IUpdateResult.unchanged
          case Failed(typeError)                 => IUpdateResult.updated(typeError, ChangeT.Failed(typeError))
        }
    }

    object UpdRes {
      case class Unchanged[T[_[_]], I, J]() extends UpdRes[T, I, J, I]

      case class UnifiedVars[T[_[_]]](
        newValue: Type1Var[T],
        newlyAddedAliases: Set[Object], // subset of newValue.aliases
      ) extends UpdRes[T, Tag.Var, Tag.Var, Tag.Var]

      case class SubstitutedForVar[T[_[_]], J](newValue: Type1T[T, J]) extends UpdRes[T, Tag.Var, J, J]

      case class AlreadyTypeApp[T[_[_]]](value: TypeApp1T[T]) extends UpdRes[T, Tag.App, Tag.App, Tag.App]

      case class AlreadyComposed[T[_[_]]](cc: Composed1T[T]) extends UpdRes[T, Tag.Comp, Tag.Comp, Tag.Comp]

      case class Failed[T[_[_]], I, J](newValue: Type1ErrorT[T]) extends UpdRes[T, I, J, Tag.Err]
    }

    implicit def domType1[T[_[_]]]
    : IDom.Aux2[
        Type1T[T, *],
        Type1T[T, *],
        UpdRes[T, *, *, ?],
        ChangeT[T, *, *],
      ] =
      new IDom[Type1T[T, *]] {
        override type IUpdate[J] = Type1T[T, J]
        override type IUpdateRes[I, J] = UpdRes[T, I, J, ?]
        override type IDelta[I, K] = ChangeT[T, I, K]

        override def iUpdate[I, J](t: Type1T[T, I], u: Type1T[T, J]): UpdRes[T, I, J, ?] =
          (t, u) match {
            case (Type1Var(aliases1), Type1Var(aliases2)) =>
              val newAliases = aliases2 diff aliases1
              UpdRes.UnifiedVars(Type1Var(aliases1 union newAliases), newAliases)

            case (Type1Var(_), u: NonVarType1T[T, J]) =>
              UpdRes.SubstitutedForVar(u)

            case (_, Type1Var(_)) =>
              UpdRes.Unchanged()

            case (ta @ TypeApp1T(f, x), TypeApp1T(g, y)) =>
              (f, g) match {
                case (ProductTypeT(), ProductTypeT()) => UpdRes.AlreadyTypeApp(ta)
                case (SumTypeT()    , SumTypeT()    ) => UpdRes.AlreadyTypeApp(ta)
                case (f             , g             ) => UpdRes.Failed(Type1ErrorT(s"Cannot unify $f with $g"))
              }

            case (cc: Composed1T[T], _: Composed1T[T]) =>
              // XXX: not considering associativity of composition
              UpdRes.AlreadyComposed(cc)

            case (Type1ErrorT(_), _) =>
              UpdRes.Unchanged()

            case (t, u) =>
              UpdRes.Failed(Type1ErrorT(s"Cannot unify ${t.show} with ${u.show}"))
          }

        override def toUpdateResult[I, J](r: IUpdateRes[I, J]): IUpdateResult[Type1T[T, *], IDelta, I, ?] =
          r.toUpdateResult

        override def composeDeltas[I1, I2, I3](ε: IDelta[I2, I3], δ: IDelta[I1, I2]): IDelta[I1, I3] =
          ???

        override def iIsFailed[I](t: Type1T[T, I]): Boolean =
          t match {
            case Type1ErrorT(_) => true
            case _                              => false
          }
      }

    implicit def show[T[_[_]], I]: Show[Type1T[T, I]] =
      ???
  }

  extension [T[_[_]]](tc: T[Type1T[T, *]]) {
    def apply(a: T[TypeT[T, *]]): NonVarTypeT[T, Tag.App] =
      TypeAppT(tc, a)

    def ∘(that: T[Type1T[T, *]]): Type1T[T, Tag.Comp] =
      Composed1T(tc, that)
  }

  /** Represents a type constructor taking 2 type parameters (`F[_, _]`). */
  sealed trait Type2T[I]

  case class ProductTypeT() extends Type2T[Tag.Prod]
  object ProductTypeT {
    def apply[T[_[_]]](a: T[TypeT[T, *]]): Type1T[T, Tag.App] =
      TypeApp1T[T](
        ProductTypeT(),
        a,
      )
  }

  case class SumTypeT() extends Type2T[Tag.Sum]
  object SumTypeT {
    def apply[T[_[_]]](a: T[TypeT[T, *]]): Type1T[T, Tag.App] =
      TypeApp1T[T](
        SumTypeT(),
        a,
      )
  }

  type Type = TypeT[Exists, ?]
  type Type1 = Type1T[Exists, ?]
  type Type2 = Type2T[?]

  type UnitType = UnitTypeT[Exists]
  object UnitType {
    def apply(): UnitType = UnitTypeT[Exists]()
  }

  type IntType = IntTypeT[Exists]
  object IntType {
    def apply(): IntType = IntTypeT[Exists]()
  }

  type StringType = StringTypeT[Exists]
  object StringType {
    def apply(): StringType = StringTypeT[Exists]()
  }

  type TypeApp = TypeAppT[Exists]
  object TypeApp {
    def apply(f: Type1, x: Type): Type =
      TypeAppT(Exists(f), Exists(x))
  }

  type FixType = FixTypeT[Exists]
  object FixType {
    def apply(f: Type1): Type =
      FixTypeT(Exists(f: Type1T[Exists, ?]))

    def unapply(ft: FixType): Tuple1[Type1] =
      Tuple1(ft.f.value)
  }

  type TypeError = TypeErrorT[Exists]
  object TypeError {
    def apply(msg: String): Type =
      TypeErrorT(msg)

    def unapply(te: TypeError): Tuple1[String] =
      Tuple1(te.msg)
  }

  extension (tc: Type1) {
    def apply(a: Type): Type =
      TypeAppT(Exists(tc), Exists(a))

    def ∘(that: Type1): Type1 =
      Composed1T(Exists(tc), Exists(that))
  }

  type TypeApp1 = TypeApp1T[Exists]
  object TypeApp1 {
    def apply(f: Type2, x: Type): Type1 =
      TypeApp1T(f, Exists(x))

    def unapply(ta: TypeApp1): (Type2, Type) =
      (ta.f, ta.x.value)
  }

  type ComposedConstructors = Composed1T[Exists]
  object ComposedConstructors {
    def apply(f: Type1, g: Type1): Type1 =
      Composed1T(Exists(f), Exists(g))

    def unapply(cc: ComposedConstructors): (Type1, Type1) =
      (cc.f.value, cc.g.value)
  }

  object ProductType {
    def apply(ta: Type): Type1 =
      ProductTypeT[Exists](Exists(ta))

    def apply(ta: Type, tb: Type): Type =
      Exists(ProductTypeT[Exists](Exists(ta)))(Exists(tb: TypeT[Exists, ?]))
  }

  object SumType {
    def apply(ta: Type): Type1 =
      SumTypeT[Exists](Exists(ta))

    def apply(ta: Type, tb: Type): Type =
      Exists(SumTypeT[Exists](Exists(ta)))(Exists(tb: TypeT[Exists, ?]))
  }

}
