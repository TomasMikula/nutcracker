package nutcracker.demo.simpletypeinference.types.generic

import nutcracker.{IDom, IUpdateResult}
import scalaz.Monad
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.kinds._
import nutcracker.demo.simpletypeinference.types.Routing

/** Tree-like structure that builds up a type (or type constructor, depending on the output kind `L`).
 *  May take type parameters, represented by the input kind `K`.
 *  Each type (constructor) has a unique representation as [[TypeExpr]] (i.e. each [[TypeExpr]] is a normal form).
 */
sealed abstract class TypeExpr[->>[_, _], K, L, I](using
  val inKind: Kind[K],
  val outKind: OutputKind[L],
) {

  def translate[-->>[_, _]](f: [k, l] => (k ->> l) => (k -->> l)): TypeExpr[-->>, K, L, I] =
    translateM[-->>, scalaz.Id.Id](f)(using scalaz.Id.id)

  def translateM[-->>[_, _], M[_]: Monad](f: [k, l] => (k ->> l) => M[k -->> l]): M[TypeExpr[-->>, K, L, I]] = {
    import TypeExpr._

    this match {
      case UnitType() => UnitType().pure[M]
      case IntType() => IntType().pure[M]
      case StringType() => StringType().pure[M]
      case Pair() => Pair().pure[M]
      case Sum() => Sum().pure[M]
      case InferenceVar(as) => InferenceVar(as).pure[M]
      case ScalaTypeParams(ps) => ScalaTypeParams(ps).pure[M]

      case AppFst(op, a) =>
        for {
          a  <- f(a)
        } yield AppFst(op.cast[-->>], a)

      case AppCompose(op, a, g) =>
        for {
          a <- f(a)
          g <- f(g)
        } yield AppCompose(op.cast[-->>], a, g)

      case BiApp(op, a, b) =>
        for {
          a  <- f(a)
          b  <- f(b)
        } yield BiApp(op.cast[-->>], a, b)

      case Fix(pre, expr) =>
        for {
          expr <- f(expr)
        } yield Fix(pre, expr)

      case PFix(pre, expr) =>
        for {
          expr <- f(expr)
        } yield PFix(pre, expr)

      case other =>
        throw new NotImplementedError(s"$other")
    }
  }
}

object TypeExpr {
  /** Marks the constructor of [[TypeExpr]]. */
  object Tag {
    sealed trait Var
    sealed trait Unit
    sealed trait Int
    sealed trait Str
    sealed trait Prod
    sealed trait Sum
    sealed trait Fix
    sealed trait PFix
    sealed trait Err
    sealed trait TPrm
    sealed trait BiApp
    sealed trait AppFst
    sealed trait AppComp
  }

  sealed abstract class BinaryOperator[->>[_, _], K1, K2, L, I](using
    k1: OutputKind[K1],
    k2: OutputKind[K2],
    l: OutputKind[L],
  ) extends TypeExpr[->>, K1 × K2, L, I] {
    given in1Kind: OutputKind[K1] = k1
    given in2Kind: OutputKind[K2] = k2

    def cast[F[_, _]]: BinaryOperator[F, K1, K2, L, I] =
      this match {
        case Pair() => Pair()
        case Sum()  => Sum()
      }
  }

  case class UnitType[->>[_, _]]() extends TypeExpr[->>, ○, ●, Tag.Unit]
  case class IntType[->>[_, _]]() extends TypeExpr[->>, ○, ●, Tag.Int]
  case class StringType[->>[_, _]]() extends TypeExpr[->>, ○, ●, Tag.Str]

  case class Pair[->>[_, _]]() extends BinaryOperator[->>, ●, ●, ●, Tag.Prod]
  case class Sum[->>[_, _]]() extends BinaryOperator[->>, ●, ●, ●, Tag.Sum]

  case class Fix[->>[_, _], K](f: Routing[●, K], g: K ->> ●) extends TypeExpr[->>, ○, ●, Tag.Fix]

  // TODO: Make the representation normalized (part of initial routing may possibly be factored out)
  case class PFix[->>[_, _], K, X](
    f: Routing[K × ●, X],
    g: X ->> ●,
  ) extends TypeExpr[->>, K, ●, Tag.PFix](using Kind.fst[K, ●](f.inKind), summon[OutputKind[●]])

  case class InferenceVar[->>[_, _]](aliases: Set[Object]) extends TypeExpr[->>, ○, ●, Tag.Var]

  case class ScalaTypeParam(filename: String, line: Int, name: String)
  case class ScalaTypeParams[->>[_, _]](values: Set[ScalaTypeParam]) extends TypeExpr[->>, ○, ●, Tag.TPrm] {
    require(values.nonEmpty)
  }
  object ScalaTypeParams {
    def one[->>[_, _]](filename: String, line: Int, name: String): ScalaTypeParams[->>] =
      ScalaTypeParams(Set(ScalaTypeParam(filename, line, name)))
  }

  case class BiApp[->>[_, _], K1, K2, L](
    op: BinaryOperator[->>, K1, K2, L, ?],
    arg1: ○ ->> K1,
    arg2: ○ ->> K2,
  ) extends TypeExpr[->>, ○, L, Tag.BiApp](using summon, op.outKind)

  case class AppFst[->>[_, _], K1, K2, L](
    op: BinaryOperator[->>, K1, K2, L, ?],
    arg1: ○ ->> K1,
  ) extends TypeExpr[->>, K2, L, Tag.AppFst](using op.in2Kind.kind, op.outKind)

  case class AppCompose[->>[_, _], K: Kind, L1, L2, M](
    op: BinaryOperator[->>, L1, L2, M, ?],
    arg1: ○ ->> L1,
    arg2: K ->> L2,
  ) extends TypeExpr[->>, K, M, Tag.AppComp](using summon, op.outKind)

  case class TypeError[->>[_, _], K: Kind, L: OutputKind](msg: String) extends TypeExpr[->>, K, L, Tag.Err]

  def newInferenceVar[->>[_, _]](): TypeExpr[->>, ○, ●, Tag.Var] =
    InferenceVar(Set(new Object))

  def pair[->>[_, _]](a: ○ ->> ●, b: ○ ->> ●): TypeExpr[->>, ○, ●, ?] =
    BiApp(Pair(), a, b)

  def sum[->>[_, _]](a: ○ ->> ●, b: ○ ->> ●): TypeExpr[->>, ○, ●, ?] =
    BiApp(Sum(), a, b)

  def pair1[->>[_, _]](a: ○ ->> ●): TypeExpr[->>, ●, ●, ?] =
    AppFst(Pair(), a)

  def sum1[->>[_, _]](a: ○ ->> ●): TypeExpr[->>, ●, ●, ?] =
    AppFst(Sum(), a)

  sealed trait UpdRes[->>[_, _], K, L, I0, J, I1] {
    import UpdRes._

    def toUpdateResult: IUpdateResult[TypeExpr[->>, K, L, *], Change[->>, K, L, *, *], I0, I1] =
      this match {
        case UpdatedAliases(nv, na)           => IUpdateResult.updated(nv, Change.UpdatedAliases(na))
        case AlreadySuperset(_)               => IUpdateResult.unchanged
        case SubstitutedForVar(nv)            => IUpdateResult.updated(nv, Change.SubstitutedForVar())
        case SubstitutedForParams(nv)         => IUpdateResult.updated(nv, Change.SubstitutedForParams())
        case AlreadyRefined(_)                => IUpdateResult.unchanged
        case AlreadyConcrete(_)               => IUpdateResult.unchanged
        case AlreadyAppFst(_, _, _)           => IUpdateResult.unchanged
        case AlreadyBiApp(_, _, _, _, _)      => IUpdateResult.unchanged
        case AlreadyAppCompose(_, _, _, _, _) => IUpdateResult.unchanged
        case AlreadyFix(_, _, _)              => IUpdateResult.unchanged
        case AlreadyPFix(_, _, _)             => IUpdateResult.unchanged
        case AlreadySameAtom()                => IUpdateResult.unchanged
        case other                            => throw new NotImplementedError(s"$other")
      }
  }

  object UpdRes {
    case class UpdatedAliases[->>[_, _]](
      newValue: InferenceVar[->>],
      newAliases: Set[Object],
    ) extends UpdRes[->>, ○, ●, Tag.Var, Tag.Var, Tag.Var] {
      require(newAliases.nonEmpty)
    }

    case class AlreadySuperset[->>[_, _]](
      value: InferenceVar[->>],
    ) extends UpdRes[->>, ○, ●, Tag.Var, Tag.Var, Tag.Var]

    case class SubstitutedForVar[->>[_, _], J](
      newValue: TypeExpr[->>, ○, ●, J],
    ) extends UpdRes[->>, ○, ●, Tag.Var, J, J]

    case class AlreadyRefined[->>[_, _], I](
      value: TypeExpr[->>, ○, ●, I],
    ) extends UpdRes[->>, ○, ●, I, Tag.Var, I]

    case class UpdatedScalaParams[->>[_, _]](
      newValue: ScalaTypeParams[->>],
      newParams: Set[ScalaTypeParam], // subset of newValue's set of params
    ) extends UpdRes[->>, ○, ●, Tag.TPrm, Tag.TPrm, Tag.TPrm]

    case class AlreadyParamSuperset[->>[_, _]](
      value: ScalaTypeParams[->>],
    ) extends UpdRes[->>, ○, ●, Tag.TPrm, Tag.TPrm, Tag.TPrm]

    case class SubstitutedForParams[->>[_, _], J](
      newValue: TypeExpr[->>, ○, ●, J],
    ) extends UpdRes[->>, ○, ●, Tag.TPrm, J, J]

    case class AlreadyConcrete[->>[_, _], I](
      value: TypeExpr[->>, ○, ●, I],
    ) extends UpdRes[->>, ○, ●, I, Tag.TPrm, I]

    case class AlreadyAppFst[->>[_, _], K1, K2, L](
      operator: BinaryOperator[->>, K1, K2, L, ?],
      arg1: ○ ->> K1,
      updArg1: ○ ->> K1,
      ) extends UpdRes[->>, K2, L, Tag.AppFst, Tag.AppFst, Tag.AppFst]

    case class AlreadyBiApp[->>[_, _], K1, K2, L](
      operator: BinaryOperator[->>, K1, K2, L, ?],
      arg1: ○ ->> K1,
      arg2: ○ ->> K2,
      updArg1: ○ ->> K1,
      updArg2: ○ ->> K2,
    ) extends UpdRes[->>, ○, L, Tag.BiApp, Tag.BiApp, Tag.BiApp]

    case class AlreadyAppCompose[->>[_, _], K, L1, L2, M](
      operator: BinaryOperator[->>, L1, L2, M, ?],
      arg1: ○ ->> L1,
      arg2: K ->> L2,
      updArg1: ○ ->> L1,
      updArg2: K ->> L2,
    ) extends UpdRes[->>, K, M, Tag.AppComp, Tag.AppComp, Tag.AppComp]

    case class AlreadyFix[->>[_, _], K](
      pre: Routing[●, K], // same in both the original value and update
      value: K ->> ●,
      update: K ->> ●,
    ) extends UpdRes[->>, ○, ●, Tag.Fix, Tag.Fix, Tag.Fix]

    case class AlreadyPFix[->>[_, _], K, X](
      pre: Routing[K × ●, X], // same in both the original value and update
      value: X ->> ●,
      update: X ->> ●,
    ) extends UpdRes[->>, K, ●, Tag.PFix, Tag.PFix, Tag.PFix]

    case class AlreadySameAtom[->>[_, _], K, L, I]() extends UpdRes[->>, K, L, I, I, I]

    case class Failed[->>[_, _], K, L, I, J](newValue: TypeError[->>, K, L]) extends UpdRes[->>, K, L, I, J, Tag.Err]
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
    case class UpdatedAliases[->>[_, _]](
      newAliases: Set[Object],
    ) extends Change[->>, ○, ●, Tag.Var, Tag.Var] {
      require(newAliases.nonEmpty)
    }

    case class SubstitutedForVar[->>[_, _], J]() extends Change[->>, ○, ●, Tag.Var, J]
    case class SubstitutedForParams[->>[_, _], J]() extends Change[->>, ○, ●, Tag.TPrm, J]
  }

  implicit def domTypeExpr[->>[_, _], K, L]
  : IDom.Aux2[
      TypeExpr[->>, K, L, *],
      TypeExpr[->>, K, L, *],
      UpdRes[->>, K, L, *, *, ?],
      Change[->>, K, L, *, *],
    ] =
    new IDom[TypeExpr[->>, K, L, *]] {
      override type IUpdate[I] = TypeExpr[->>, K, L, I]
      override type IUpdateRes[I, J] = UpdRes[->>, K, L, I, J, ?]
      override type IDelta[I1, I2] = Change[->>, K, L, I1, I2]

      override def iUpdate[I, J](t: TypeExpr[->>, K, L, I], u: TypeExpr[->>, K, L, J]): IUpdateRes[I, J] = {
        import t.{inKind, outKind}

        (t, u) match {
          case (t @ InferenceVar(aliases1), InferenceVar(aliases2)) =>
            val newAliases = aliases2 diff aliases1
            if (newAliases.nonEmpty) {
              val newVal = InferenceVar[->>](aliases1 union newAliases)
              UpdRes.UpdatedAliases(newVal, newAliases)
            } else {
              UpdRes.AlreadySuperset(t)
            }

          case (InferenceVar(_), u) =>
            UpdRes.SubstitutedForVar(u)

          case (t, InferenceVar(_)) =>
            UpdRes.AlreadyRefined(t)

          case (t @ ScalaTypeParams(ps), ScalaTypeParams(qs)) =>
            val newParams = qs diff ps
            if (newParams.nonEmpty) {
              val newVal = ScalaTypeParams[->>](ps union newParams)
              UpdRes.UpdatedScalaParams(newVal, newParams)
            } else {
              UpdRes.AlreadyParamSuperset(t)
            }

          case (ScalaTypeParams(_), u) =>
            UpdRes.SubstitutedForParams(u)

          case (t, ScalaTypeParams(_)) =>
            UpdRes.AlreadyConcrete(t)

          case (t @ AppFst(_, _), u @ AppFst(_, _)) =>
            def go[X, Y](t: AppFst[->>, X, K, L], u: AppFst[->>, Y, K, L]): IUpdateRes[I, J] =
              if (t.op == u.op) {
                UpdRes.AlreadyAppFst(t.op, t.arg1, u.arg1.asInstanceOf[○ ->> X]) // TODO: derive X =:= Y, instead of coersion
              } else {
                UpdRes.Failed(TypeError(s"Cannot unify $t with $u, because ${t.op} != ${u.op}"))
              }

            go(t, u)

          case (t @ BiApp(_, _, _), u @ BiApp(_, _, _)) =>
            def go[X1, X2, Y1, Y2](t: BiApp[->>, X1, X2, L], u: BiApp[->>, Y1, Y2, L]): IUpdateRes[I, J] =
              if (t.op == u.op)
                UpdRes.AlreadyBiApp(t.op, t.arg1, t.arg2, u.arg1.asInstanceOf[○ ->> X1], u.arg2.asInstanceOf[○ ->> X2]) // TODO: derive X1 =:= Y1, X2 =:= Y2 instead of coersion
              else
                UpdRes.Failed(TypeError(s"Cannot unify $t with $u, because ${t.op} != ${u.op}"))

            go(t, u)

          case (t @ AppCompose(_, _, _), u @ AppCompose(_, _, _)) =>
            def go[X1, X2, Y1, Y2](t: AppCompose[->>, K, X1, X2, L], u: AppCompose[->>, K, Y1, Y2, L]): IUpdateRes[I, J] =
              if (t.op == u.op) {
                UpdRes.AlreadyAppCompose(t.op, t.arg1, t.arg2, u.arg1.asInstanceOf[○ ->> X1], u.arg2.asInstanceOf[K ->> X2]) // TODO: derive X1 =:= Y1, X2 =:= Y2 instead of coersion
              } else {
                UpdRes.Failed(TypeError(s"Cannot unify $t with $u, because ${t.op} != ${u.op}"))
              }

            go(t, u)

          case (t @ Fix(_, _), u @ Fix(_, _)) =>
            def go[X, Y](t: Fix[->>, X], u: Fix[->>, Y]): IUpdateRes[I, J] =
              if (t.f == u.f) // TODO: implement and use proper equality testing
                UpdRes.AlreadyFix(t.f, t.g, u.g.asInstanceOf[X ->> ●]) // TODO: derive X =:= Y instead of coersion
              else
                UpdRes.Failed(TypeError(s"Cannot unify $t with $u, because ${t.f} != ${u.f}"))

            go(t, u)

          case (t @ PFix(_, _), u @ PFix(_, _)) =>
            def go[X, Y](t: PFix[->>, K, X], u: PFix[->>, K, Y]): IUpdateRes[I, J] =
              if (t.f == u.f) { // TODO: implement and use proper equality testing
                UpdRes.AlreadyPFix(t.f, t.g, u.g.asInstanceOf[X ->> ●]) // TODO: derive X =:= Y instead of coersion
              } else {
                UpdRes.Failed(TypeError(s"Cannot unify $t with $u, because ${t.f} != ${u.f}"))
              }

            go(t, u)

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
            UpdRes.Failed(TypeError(s"Unification of $f and $g impossible or not implemented"))
        }
      }

      override def toUpdateResult[I, J](r: IUpdateRes[I, J]): IUpdateResult[TypeExpr[->>, K, L, *], IDelta, I, ?] =
        r.toUpdateResult

      override def composeDeltas[I1, I2, I3](ε: IDelta[I2, I3], δ: IDelta[I1, I2]): IDelta[I1, I3] =
        δ andThen ε

      override def iIsFailed[I](f: TypeExpr[->>, K, L, I]): Boolean =
        f match {
          case TypeError(_) => true
          case _            => false
        }
    }
}
