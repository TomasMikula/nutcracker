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
    sealed trait PFix
    sealed trait Comp
    sealed trait Err
    sealed trait Id
    sealed trait Par
    sealed trait Dup
    sealed trait IFst
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
      case class UnifiedVars[T[_[_]]](newAliases: Set[Object]) extends ChangeT[T, Tag.Var, Tag.Var] {
        require(newAliases.nonEmpty)
      }
      case class SubstitutedForVar[T[_[_]], J](newValue: Type1T[T, J]) extends ChangeT[T, Tag.Var, J]
      case class Failed[T[_[_]], I](newValue: Type1ErrorT[T]) extends ChangeT[T, I, Tag.Err]
    }

    sealed trait UpdRes[T[_[_]], I, J, K] {
      import UpdRes._

      type Change[X, Y] = ChangeT[T, X, Y]

      def toUpdateResult: IUpdateResult[Type1T[T, *], ChangeT[T, *, *], I, K] =
        this match {
          case Unchanged()                       => IUpdateResult.unchanged
          case SubstitutedForVar(newValue)       => IUpdateResult.updated(newValue, ChangeT.SubstitutedForVar(newValue))
          case AlreadyTypeApp(_)                 => IUpdateResult.unchanged
          case AlreadyComposed(_)                => IUpdateResult.unchanged
          case Failed(typeError)                 => IUpdateResult.updated(typeError, ChangeT.Failed(typeError))
          case UnifiedVars(newValue, newAliases) =>
            if (newAliases.nonEmpty)
              IUpdateResult.updated(newValue, ChangeT.UnifiedVars(newAliases))
            else
              IUpdateResult.unchanged
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

  type Composed1 = Composed1T[Exists]
  object Composed1 {
    def apply(f: Type1, g: Type1): Type1 =
      Composed1T(Exists(f), Exists(g))

    def unapply(cc: Composed1): (Type1, Type1) =
      (cc.f.value, cc.g.value)
  }

  type Type1Error = Type1ErrorT[Exists]
  object Type1Error {
    def apply(msg: String): Type1 =
      Type1ErrorT(msg)

    def unapply(te: Type1Error): Tuple1[String] =
      Tuple1(te.msg)
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

  /** Phantom type representing the kind of types. Unicode character U+25CF */
  sealed trait ●

  /** Phantom type representing a pair of kinds. Unicode character U+00D7. */
  sealed trait ×[K, L]

  /** Phantom type representing the "unit" kind. Neutral element for [[×]]. Unicode character U+25CB. */
  sealed trait ○

  /** Phantom type representing the kinds of type functions. Internal hom in the category [[TypeFun]].
   *
   * @tparam K input kind
   * @tparam L output kind
   */
  // sealed trait ->[K, L]

  sealed trait Kind[K] {
    def testEqual[L](that: Kind[L]): Option[K =:= L] =
      (this, that) match {
        case (Kind.Unit, Kind.Unit) =>
          Some(implicitly[○ =:= ○])
        case (Kind.Type, Kind.Type) =>
          Some(implicitly[● =:= ●])
        case (Kind.Prod(a, b), Kind.Prod(x, y)) =>
          (a testEqual x, b testEqual y) match {
            case (Some(ax), Some(by)) =>
              Some(implicitly[K =:= K].asInstanceOf[K =:= L])
            case _ =>
              None
          }
        case _ =>
          None
      }
  }

  object Kind {
    case object Unit extends Kind[○] {
      override def toString = "○"
    }
    case object Type extends Kind[●] {
      override def toString = "●"
    }
    case class Prod[K, L](k: Kind[K], l: Kind[L]) extends Kind[K × L] {
      override def toString = s"($k × $l)"
    }

    given Kind[○] = Unit
    given Kind[●] = Type
    given [K, L](using k: Kind[K], l: Kind[L]): Kind[K × L] = Prod(k, l)
  }

  object generic {
    /**
     * @tparam ->> type of nested arrows
     * @tparam K input kind
     * @tparam L output kind
     * @tparam I marks the constructor of this [[TypeFun]]
     */
    sealed trait TypeFun[->>[_, _], K, L, I](using k: Kind[K], l: Kind[L]) {
      given inKind: Kind[K] = k
      given outKind: Kind[L] = l
    }

    object TypeFun {
      case class Pair[->>[_, _]]() extends TypeFun[->>, ● × ●, ●, Tag.Prod]
      case class Sum[->>[_, _]]() extends TypeFun[->>, ● × ●, ●, Tag.Sum]

      case class AndThen[->>[_, _], K: Kind, L, M: Kind](
        f: K ->> L,
        g: L ->> M,
      )(using
        l: Kind[L],
      ) extends TypeFun[->>, K, M, Tag.Comp] {
        given pivotKind: Kind[L] = l
      }

      case class Id[->>[_, _], K: Kind]() extends TypeFun[->>, K, K, Tag.Id]

      case class Par[->>[_, _], K1: Kind, K2: Kind, L1: Kind, L2: Kind](
        f: K1 ->> L1,
        g: K2 ->> L2,
      ) extends TypeFun[->>, K1 × K2, L1 × L2, Tag.Par]

      case class IntroFst[->>[_, _], K: Kind]() extends TypeFun[->>, K, ○ × K, Tag.IFst]

      case class Dup[->>[_, _], K: Kind]() extends TypeFun[->>, K, K × K, Tag.Dup]

      // Could be generalized to Fix[->>[_, _], K](f: K ->> K) extends TypeFun[->>, ○, K, Tag.Fix]
      case class Fix[->>[_, _]](f: ● ->> ●) extends TypeFun[->>, ○, ●, Tag.Fix]

      // Could be generalized to PFix[->>[_, _], K, L](f: (K × L) ->> L) extends TypeFun[->>, K, L, Tag.PFix]
      case class PFix[->>[_, _]](f: (● × ●) ->> ●) extends TypeFun[->>, ●, ●, Tag.PFix]

      case class UnitType[->>[_, _]]() extends TypeFun[->>, ○, ●, Tag.Unit]

      case class IntType[->>[_, _]]() extends TypeFun[->>, ○, ●, Tag.Int]

      case class StringType[->>[_, _]]() extends TypeFun[->>, ○, ●, Tag.Str]

      case class TypeError[->>[_, _], K: Kind, L: Kind](msg: String) extends TypeFun[->>, K, L, Tag.Err]

      case class Var[->>[_, _], K: Kind, L: Kind](aliases: Set[Object]) extends TypeFun[->>, K, L, Tag.Var]

      def newVar[->>[_, _], K: Kind, L: Kind](): TypeFun[->>, K, L, Tag.Var] =
        Var(Set(new Object))

      sealed trait UpdRes[->>[_, _], K, L, I0, J, I1] {
        import UpdRes._

        def toUpdateResult: IUpdateResult[TypeFun[->>, K, L, *], Change[->>, K, L, *, *], I0, I1] =
          this match {
            case UpdatedAliases(nv, na) => IUpdateResult.updated(nv, Change.UpdatedAliases(na))
            case AlreadySuperset(_)     => IUpdateResult.unchanged
            case SubstitutedForVar(nv)  => IUpdateResult.updated(nv, Change.SubstitutedForVar())
            case AlreadyRefined(_)      => IUpdateResult.unchanged
            case AlreadyAndThen(_)      => IUpdateResult.unchanged
            case AlreadyPar(_)          => IUpdateResult.unchanged
            case AlreadyFix(_)          => IUpdateResult.unchanged
            case AlreadySameAtom()      => IUpdateResult.unchanged
            case other                  => throw new NotImplementedError(s"$other")
          }
      }

      object UpdRes {
        case class UpdatedAliases[->>[_, _], K, L](
          newValue: Var[->>, K, L],
          newAliases: Set[Object],
        ) extends UpdRes[->>, K, L, Tag.Var, Tag.Var, Tag.Var] {
          require(newAliases.nonEmpty)
        }

        case class AlreadySuperset[->>[_, _], K, L](
          value: Var[->>, K, L],
        ) extends UpdRes[->>, K, L, Tag.Var, Tag.Var, Tag.Var]

        case class SubstitutedForVar[->>[_, _], K, L, J](
          newValue: TypeFun[->>, K, L, J],
        ) extends UpdRes[->>, K, L, Tag.Var, J, J]

        case class AlreadyRefined[->>[_, _], K, L, I](
          value: TypeFun[->>, K, L, I],
        ) extends UpdRes[->>, K, L, I, Tag.Var, I]

        case class AlreadyAndThen[->>[_, _], K, L, M](
          value: TypeFun.AndThen[->>, K, L, M],
        ) extends UpdRes[->>, K, M, Tag.Comp, Tag.Comp, Tag.Comp]

        case class AlreadyPar[->>[_, _], K1, K2, L1, L2](
          value: TypeFun.Par[->>, K1, K2, L1, L2],
        ) extends UpdRes[->>, K1 × K2, L1 × L2, Tag.Par, Tag.Par, Tag.Par]

        case class AlreadyFix[->>[_, _]](
          value: TypeFun.Fix[->>],
        ) extends UpdRes[->>, ○, ●, Tag.Fix, Tag.Fix, Tag.Fix]

        case class AlreadySameAtom[->>[_, _], K, L, I]() extends UpdRes[->>, K, L, I, I, I]
      }

      sealed trait Change[->>[_, _], K, L, I0, I1] {
        import Change._

        def andThen[I2](that: Change[->>, K, L, I1, I2]): Change[->>, K, L, I0, I2] =
          (this, that) match {
            case (UpdatedAliases(as1), UpdatedAliases(as2)) => UpdatedAliases(as1 union as2)
            case (UpdatedAliases(_), SubstitutedForVar())   => SubstitutedForVar()
            case (x, y) => throw new NotImplementedError(s"$x, $y")
          }
      }

      object Change {
        case class UpdatedAliases[->>[_, _], K, L](
          newAliases: Set[Object],
        ) extends Change[->>, K, L, Tag.Var, Tag.Var] {
          require(newAliases.nonEmpty)
        }

        case class SubstitutedForVar[->>[_, _], K, L, J]() extends Change[->>, K, L,Tag.Var, J]
      }

      implicit def domTypeFun[->>[_, _], K, L]
      : IDom.Aux2[
          TypeFun[->>, K, L, *],
          TypeFun[->>, K, L, *],
          UpdRes[->>, K, L, *, *, ?],
          Change[->>, K, L, *, *],
        ] =
        new IDom[TypeFun[->>, K, L, *]] {
          override type IUpdate[I] = TypeFun[->>, K, L, I]
          override type IUpdateRes[I, J] = UpdRes[->>, K, L, I, J, ?]
          override type IDelta[I1, I2] = Change[->>, K, L, I1, I2]

          override def iUpdate[I, J](t: TypeFun[->>, K, L, I], u: TypeFun[->>, K, L, J]): IUpdateRes[I, J] = {
            import t.{inKind, outKind}

            (t, u) match {
              case (t @ Var(aliases1), Var(aliases2)) =>
                val newAliases = aliases2 diff aliases1
                if (newAliases.nonEmpty) {
                  val newVal = Var[->>, K, L](aliases1 union newAliases)
                  UpdRes.UpdatedAliases(newVal, newAliases)
                } else {
                  UpdRes.AlreadySuperset(t)
                }

              case (Var(_), u) =>
                UpdRes.SubstitutedForVar(u)

              case (t, Var(_)) =>
                UpdRes.AlreadyRefined(t)

              case (t @ AndThen(_, _), AndThen(_, _)) =>
                UpdRes.AlreadyAndThen(t)

              case (t @ Par(_, _), Par(_, _)) =>
                UpdRes.AlreadyPar(t)

              case (t @ Fix(_), Fix(_)) =>
                UpdRes.AlreadyFix(t)

              case (Dup(), Dup()) =>
                UpdRes.AlreadySameAtom()

              case (Sum(), Sum()) =>
                UpdRes.AlreadySameAtom()

              case (Pair(), Pair()) =>
                UpdRes.AlreadySameAtom()

              case (UnitType(), UnitType()) =>
                UpdRes.AlreadySameAtom()

              case (IntType(), IntType()) =>
                UpdRes.AlreadySameAtom()

              case (StringType(), StringType()) =>
                UpdRes.AlreadySameAtom()

              case (f, g) =>
                throw new NotImplementedError(s"Unhandled update of $f by $g")
            }
          }

          override def toUpdateResult[I, J](r: IUpdateRes[I, J]): IUpdateResult[TypeFun[->>, K, L, *], IDelta, I, ?] =
            r.toUpdateResult

          override def composeDeltas[I1, I2, I3](ε: IDelta[I2, I3], δ: IDelta[I1, I2]): IDelta[I1, I3] =
            δ andThen ε

          override def iIsFailed[I](f: TypeFun[->>, K, L, I]): Boolean =
            f match {
              case TypeError(_) => true
              case _            => false
            }
        }
    }
  }

  /**
   * @tparam K input kind
   * @tparam L output kind
   */
  case class TypeFun[K, L](value: generic.TypeFun[TypeFun, K, L, ?]) {
    export value.{inKind, outKind}

    /** Sequential composition. */
    def >[M](that: TypeFun[L, M]): TypeFun[K, M] = {
      import that.outKind
      TypeFun(generic.TypeFun.AndThen(this, that))
    }

    def ∘[J](that: TypeFun[J, K]): TypeFun[J, L] =
      that > this

    override def toString: String =
      value.toString
  }

  object TypeFun {
    def id[K: Kind]: TypeFun[K, K] = TypeFun(generic.TypeFun.Id())

    def pair: TypeFun[● × ●, ●] = TypeFun(generic.TypeFun.Pair())
    def sum: TypeFun[● × ●, ●] = TypeFun(generic.TypeFun.Sum())

    def par[K1, K2, L1, L2](f: TypeFun[K1, L1], g: TypeFun[K2, L2]): TypeFun[K1 × K2, L1 × L2] = {
      import f.{inKind, outKind}; import g.{inKind, outKind}
      TypeFun(generic.TypeFun.Par(f, g))
    }

    def fst[K, L, M: Kind](f: TypeFun[K, L]): TypeFun[K × M, L × M] =
      par(f, id)

    def introFst[K: Kind]: TypeFun[K, ○ × K] =
      TypeFun(generic.TypeFun.IntroFst())

    def introFst[K: Kind, L](f: TypeFun[○, L]): TypeFun[K, L × K] =
      introFst[K] > fst(f)

    def dup[K: Kind]: TypeFun[K, K × K] = TypeFun(generic.TypeFun.Dup())

    def fix(f: TypeFun[●, ●]): TypeFun[○, ●] =
      TypeFun(generic.TypeFun.Fix(f))

    def pfix(f: TypeFun[● × ●, ●]): TypeFun[●, ●] =
      TypeFun(generic.TypeFun.PFix(f))

    def unit: TypeFun[○, ●] =
      TypeFun(generic.TypeFun.UnitType())

    def int: TypeFun[○, ●] =
      TypeFun(generic.TypeFun.IntType())

    def string: TypeFun[○, ●] =
      TypeFun(generic.TypeFun.StringType())

    def pair1(a: Typ): TypeFun[●, ●] =
      introFst(a) > pair

    def sum1(a: Typ): TypeFun[●, ●] =
      introFst(a) > sum

    def variable[K: Kind, L: Kind](aliases: Set[Object]): TypeFun[K, L] =
      TypeFun(generic.TypeFun.Var(aliases))

    def typeError[K: Kind, L: Kind](msg: String): TypeFun[K, L] =
      TypeFun(generic.TypeFun.TypeError(msg))
  }

  type Typ = TypeFun[○, ●]
  object Typ {
    def unit   : Typ = TypeFun.unit
    def int    : Typ = TypeFun.int
    def string : Typ = TypeFun.string

    def sum(a: Typ, b: Typ): Typ =
      TypeFun.dup[○] > TypeFun.par(a, b) > TypeFun.sum

    def fix(f: TypeFun[●, ●]): Typ =
      TypeFun.fix(f)
  }

}
