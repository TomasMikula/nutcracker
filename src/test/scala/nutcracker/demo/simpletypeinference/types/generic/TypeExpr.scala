package nutcracker.demo.simpletypeinference.types.generic

import nutcracker.{IDom, IUpdateResult}
import scalaz.Monad
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.kinds._
import nutcracker.demo.simpletypeinference.types.{ArgIntro, ArgTrans, Routing}
import nutcracker.demo.simpletypeinference.types.ArgTrans.Wrap

/** Tree-like structure that builds up a type (or type constructor, depending on the output kind `L`).
 *  May take type parameters, represented by the input kind `K`.
 *  Each type (constructor) has a unique representation as [[TypeExpr]] (i.e. each [[TypeExpr]] is a normal form).
 */
sealed abstract class TypeExpr[->>[_, _], K, L, I](using
  val inKind: Kind[K],
  val outKind: OutputKind[L],
) {
  import TypeExpr._

  def from[J](using ev: J =:= K): TypeExpr[->>, J, L, I] =
    ev.substituteContra[TypeExpr[->>, *, L, I]](this)

  def to[M](using ev: L =:= M): TypeExpr[->>, K, M, I] =
    ev.substituteCo[TypeExpr[->>, K, *, I]](this)

  def translate[-->>[_, _]](f: [k, l] => (k ->> l) => (k -->> l)): TypeExpr[-->>, K, L, I] =
    translateM[-->>, scalaz.Id.Id](f)(using scalaz.Id.id)

  def translateM[-->>[_, _], M[_]: Monad](f: [k, l] => (k ->> l) => M[k -->> l]): M[TypeExpr[-->>, K, L, I]] = {
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

      case ac @ AppCompose(op, a, g) =>
        import ac.properInKind
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

      case pf @ PFix(pre, expr) =>
        import pf.properInKind
        for {
          expr <- f(expr)
        } yield PFix(pre, expr)

      case other =>
        throw new NotImplementedError(s"$other")
    }
  }

  def transApply[F[_, _], J](
    a: ArgIntro[F[○, *], J, K],
    trans: [k, l] => (k ->> l) => F[k, l],
    transApp: [j, k, l] => (k ->> l, ArgIntro[F[○, *], j, k]) => F[j, l],
  ): TypeExpr[F, J, L, ?] =
    transApplyM[F, J, scalaz.Id.Id](a, trans, transApp)(using scalaz.Id.id)

  def transApplyM[F[_, _], J, M[_]: Monad](
    a: ArgIntro[F[○, *], J, K],
    trans: [k, l] => (k ->> l) => F[k, l],
    transApp: [j, k, l] => (k ->> l, ArgIntro[F[○, *], j, k]) => M[F[j, l]],
  ): M[TypeExpr[F, J, L, ?]] =
    a.inKind.properKind match {
      case Left(j_eq_○) =>
        j_eq_○.substituteContra[[j] =>> M[TypeExpr[F, j, L, ?]]](
          transApply0M(
            j_eq_○.substituteCo[ArgIntro[F[○, *], *, K]](a),
            trans,
            transApp,
          )
        )
      case Right(j) =>
        transApply1M(a, trans, transApp)(using j, summon[Monad[M]])
    }

  private def transApply0M[F[_, _], M[_]: Monad](
    args: ArgIntro[F[○, *], ○, K],
    trans: [k, l] => (k ->> l) => F[k, l],
    transApp: [j, k, l] => (k ->> l, ArgIntro[F[○, *], j, k]) => M[F[j, l]],
  ): M[TypeExpr[F, ○, L, ?]] = {
    this match {
      case Pair() =>
        args match {
          case ArgIntro.IntroBoth(a1, a2) =>
            BiApp(Pair[F](), ArgIntro.unwrap(a1), ArgIntro.unwrap(a2)).pure[M]
          case other =>
            throw new NotImplementedError(s"$other")
        }

      case AppFst(op, arg1) =>
        import op.in2Kind
        val arg2 = ArgIntro.unwrap(args)
        Monad[M].pure(BiApp(op.cast, trans(arg1), arg2))

      case AppCompose(op, a, g) =>
        transApp(g, args).map(BiApp(op.cast, trans(a), _))

      case PFix(pre, expr) =>
        val a: ArgIntro[F[○, *], ●, K × ●] = ArgIntro.introFst(args)
        pre.applyTo(a) match {
          case Routing.ApplyRes(r, a1) =>
            transApp(expr, a1).map(Fix(r, _))
        }

      case other =>
        throw new NotImplementedError(s"Applying $other to $args")
    }
  }

  private def transApply1M[F[_, _], J: ProperKind, M[_]: Monad](
    args: ArgIntro[F[○, *], J, K],
    trans: [k, l] => (k ->> l) => F[k, l],
    transApp: [j, k, l] => (k ->> l, ArgIntro[F[○, *], j, k]) => M[F[j, l]],
  ): M[TypeExpr[F, J, L, ?]] = {
    this match {
      case ComposeSnd(op, g) =>
        args match {
          case ArgIntro.IntroFst(a, f) =>
            import op.in1Kind
            transApp(g, f)
              .map(AppCompose(op.cast, ArgIntro.unwrap(a), _))
          case other =>
            throw new NotImplementedError(s"$other")
        }

      case AppCompose(op, a, g) =>
        transApp(g, args).map(AppCompose(op.cast, trans(a), _))

      case Pair() =>
        args match {
          case ArgIntro.Id() =>
            Pair().pure[M]
          case ArgIntro.IntroFst(a, f) =>
            pair1(ArgIntro.unwrap(a))
              .from(using ArgIntro.deriveId(f))
              .pure[M]
          case ArgIntro.IntroSnd(f, a) =>
            pair2(ArgIntro.unwrap(a))
              .from(using ArgIntro.deriveId(f))
              .pure[M]
          case other =>
            throw new NotImplementedError(s"$other")
        }

      case other =>
        throw new NotImplementedError(s"Applying $other to $args")
    }
  }

  def transCompose[A[_, _], J](
    a: ArgTrans[A, J, K],
    trans: [k, l] => (k ->> l) => A[k, l],
    transComp: [j, k, l] => (k ->> l, ArgTrans[A, j, k]) => A[j, l],
  ): TypeExpr[A, J, L, ?] =
    transComposeM[A, J, scalaz.Id.Id](a, trans, transComp)(using scalaz.Id.id)

  def transComposeM[A[_, _], J, M[_]: Monad](
    a: ArgTrans[A, J, K],
    trans: [k, l] => (k ->> l) => A[k, l],
    transComp: [j, k, l] => (k ->> l, ArgTrans[A, j, k]) => M[A[j, l]],
  ): M[TypeExpr[A, J, L, ?]] =
    a.inKind.properKind match {
      case Left(j_eq_○) =>
        j_eq_○.substituteContra[[j] =>> M[TypeExpr[A, j, L, ?]]](
          transCompose0M(
            j_eq_○.substituteCo[ArgTrans[A, *, K]](a),
            trans,
            transComp,
          )
        )
      case Right(j) =>
        transCompose1M(a, trans, transComp)(using j, summon[Monad[M]])
    }

  private def transCompose0M[F[_, _], M[_]: Monad](
    f: ArgTrans[F, ○, K],
    trans: [k, l] => (k ->> l) => F[k, l],
    transApp: [j, k, l] => (k ->> l, ArgTrans[F, j, k]) => M[F[j, l]],
  ): M[TypeExpr[F, ○, L, ?]] =
    this match {
      case PFix(p, e) =>
        p.applyToTrans(ArgTrans.introFst(f)) match {
          case Routing.AppTransRes(q, g) =>
            transApp(e, g)
              .map(Fix(q, _))
        }
      case AppSnd(op, b) =>
        f match {
          case ArgTrans.Wrap(a) =>
            BiApp(op.cast[F], a, trans(b)).pure[M]
          case other =>
            throw new NotImplementedError(s"$other")
        }
      case AppCompose(op, a, g) =>
        transApp(g, f)
          .map(BiApp(op.cast[F], trans(a), _))
      case other =>
        throw new NotImplementedError(s"$other")
    }

  private def transCompose1M[F[_, _], J: ProperKind, M[_]: Monad](
    f: ArgTrans[F, J, K],
    trans: [k, l] => (k ->> l) => F[k, l],
    transComp: [j, k, l] => (k ->> l, ArgTrans[F, j, k]) => M[F[j, l]],
  ): M[TypeExpr[F, J, L, ?]] = {

    def goOp[K1, K2](
      op: BinaryOperator[->>, K1, K2, L, ?],
      f: ArgTrans[F, J, K1 × K2],
    ): M[TypeExpr[F, J, L, ?]] = {
      import op.in1Kind
      import op.in2Kind

      f match {
        case snd @ ArgTrans.Snd(f2) =>
          composeSnd(op.cast[F], ArgTrans.unwrap(f2))(using snd.in2Kind).pure[M]
        case ArgTrans.IntroFst(f1) =>
          AppFst(op.cast[F], ArgTrans.unwrap(f1)).pure[M]
        case other =>
          throw new NotImplementedError(s"$other")
      }
    }

    this match {
      case Pair() =>
        goOp(Pair(), f)
      case Sum() =>
        goOp(Sum(), f)
      case AppFst(op, a) =>
        f match {
          case ArgTrans.Wrap(h) =>
            appCompose(op.cast[F], trans(a), h).pure[M]
          case other =>
            throw new NotImplementedError(s"$other")
        }
      case AppCompose(op, a, g) =>
        transComp(g, f)
          .map(AppCompose(op.cast[F], trans(a), _))
      case ComposeSnd(op, g) =>
        import op.in1Kind
        f match {
          case ArgTrans.IntroFst(f1) =>
            appCompose(op.cast[F], ArgTrans.unwrap(f1), trans(g)).pure[M]
          case other =>
            throw new NotImplementedError(s"$other")
        }
      case other =>
        throw new NotImplementedError(s"Composing $other after $f")
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
    sealed trait AppSnd
    sealed trait CompSnd
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
  )(using
    val properInKind: ProperKind[K],
  ) extends TypeExpr[->>, K, ●, Tag.PFix]

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

  case class AppSnd[->>[_, _], K1, K2, L](
    op: BinaryOperator[->>, K1, K2, L, ?],
    arg2: ○ ->> K2,
  ) extends TypeExpr[->>, K1, L, Tag.AppSnd](using op.in1Kind.kind, op.outKind)

  case class ComposeSnd[->>[_, _], K1, K2: ProperKind, L2, M](
    op: BinaryOperator[->>, K1, L2, M, ?],
    arg2: K2 ->> L2,
  ) extends TypeExpr[->>, K1 × K2, M, Tag.CompSnd](using
    (Kind.fst(op.inKind) × ProperKind[K2]).kind,
    op.outKind,
  )

  case class AppCompose[->>[_, _], K, L1, L2, M](
    op: BinaryOperator[->>, L1, L2, M, ?],
    arg1: ○ ->> L1,
    arg2: K ->> L2,
  )(using
    val properInKind: ProperKind[K],
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

  def pair2[->>[_, _]](b: ○ ->> ●): TypeExpr[->>, ●, ●, ?] =
    AppSnd(Pair(), b)

  def sum1[->>[_, _]](a: ○ ->> ●): TypeExpr[->>, ●, ●, ?] =
    AppFst(Sum(), a)

  def sum2[->>[_, _]](b: ○ ->> ●): TypeExpr[->>, ●, ●, ?] =
    AppSnd(Sum(), b)

  def appCompose[->>[_, _], K, L1, L2, M](
    op: BinaryOperator[->>, L1, L2, M, ?],
    arg1: ○ ->> L1,
    arg2: K ->> L2,
  )(using
    k: ProperKind[K],
  ): TypeExpr[->>, K, M, Tag.AppComp] =
    AppCompose(op, arg1, arg2)

  def composeSnd[->>[_, _], K1, K2: ProperKind, L2, M](
    op: BinaryOperator[->>, K1, L2, M, ?],
    arg2: K2 ->> L2,
  ): TypeExpr[->>, K1 × K2, M, ?] =
    ComposeSnd(op, arg2)

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
