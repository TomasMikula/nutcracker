package nutcracker.demo.simpletypeinference

import nutcracker.{IDom, IUpdateResult}
import nutcracker.util.Exists
import nutcracker.util.typealigned.{ANone, AOption, ASome}
import scalaz.{Monad, Show}
import scalaz.syntax.monad._
import scalaz.syntax.show._

object types {

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
    given [K](using k: OutputKind[K]): Kind[K] = k.kind

    def fst[K, L](kl: Kind[K × L]): Kind[K] =
      kl match {
        case Prod(k, l) => k
      }

    private def impossible(msg: String): Nothing =
      throw new AssertionError(msg)
  }

  /** Witnesses that `K` is a legal output kind of type functions. */
  sealed trait OutputKind[K] {
    def kind: Kind[K] =
      this match {
        case OutputKind.Type => Kind.Type
      }
  }
  object OutputKind {
    case object Type extends OutputKind[●] {
      override def toString = "●"
    }

    given OutputKind[●] = Type
  }

  object generic {
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

    sealed trait Routing[K, L](using val inKind: Kind[K], val outKind: Kind[L])
    object Routing {
      case class Id[K: Kind]() extends Routing[K, K]

      def id[K: Kind]: Routing[K, K] =
        Id()

      def proveId[K](r: Routing[○, K]): K =:= ○ =
        r match {
          case Id() => implicitly[K =:= ○]
        }
    }
  }

  sealed trait TypeFun[K, L] {
    type X
    def pre: generic.Routing[K, X]
    def expr: TypeExpr[X, L]

    def ∘[J](that: TypeFun[J, K]): TypeFun[J, L] =
      this.pre match {
        case generic.Routing.Id() => TypeFun(that.pre, this.expr ∘ that.expr)
      }
  }

  object TypeFun {
    def apply[K, P, L](r: generic.Routing[K, P], f: TypeExpr[P, L]): TypeFun[K, L] =
      new TypeFun[K, L] {
        override type X = P
        override def pre = r
        override def expr = f
      }

    def unapply[K, L](f: TypeFun[K, L]): (generic.Routing[K, f.X], TypeExpr[f.X, L]) =
      (f.pre, f.expr)

    def fromExpr[K, L](e: TypeExpr[K, L]): TypeFun[K, L] = {
      import e.inKind
      TypeFun(generic.Routing.id[K], e)
    }

    def toExpr[L](f: TypeFun[○, L]): TypeExpr[○, L] =
      generic.Routing.proveId(f.pre).substituteCo[TypeExpr[*, L]](f.expr)

    def unit: TypeFun[○, ●] =
      fromExpr(TypeExpr.unit)

    def int: TypeFun[○, ●] =
      fromExpr(TypeExpr.int)

    def string: TypeFun[○, ●] =
      fromExpr(TypeExpr.string)

    def pair: TypeFun[● × ●, ●] =
      fromExpr(TypeExpr.pair)

    def pair(a: TypeFun[○, ●], b: TypeFun[○, ●]): TypeFun[○, ●] =
      fromExpr(TypeExpr.pair(toExpr(a), toExpr(b)))

    def pair1(a: Type): TypeFun[●, ●] =
      fromExpr(TypeExpr.pair1(a))

    def pair1(a: TypeFun[○, ●]): TypeFun[●, ●] =
      pair1(toExpr(a))

    def sum(a: TypeFun[○, ●], b: TypeFun[○, ●]): TypeFun[○, ●] =
      fromExpr(TypeExpr.sum(toExpr(a), toExpr(b)))

    def sum1(a: Type): TypeFun[●, ●] =
      fromExpr(TypeExpr.sum1(a))

    def sum1(a: TypeFun[○, ●]): TypeFun[●, ●] =
      sum1(toExpr(a))

    def fix(f: TypeFun[●, ●]): TypeFun[○, ●] =
      f match {
        case TypeFun(pre, expr) => fromExpr(TypeExpr.fix(pre, expr))
      }

    def pfix(f: TypeFun[● × ●, ●]): TypeFun[●, ●] =
      f match {
        case TypeFun(pre, expr) => fromExpr(TypeExpr.pfix(pre, expr))
      }

    def scalaTypeParam[T](filename: String, line: Int, name: String): TypeFun[○, ●] =
      fromExpr(TypeExpr.scalaTypeParam(filename, line, name))
  }

  case class AnnotatedTypeExpr[A[_, _], K, L](annotation: A[K, L], value: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], K, L, ?])
  object AnnotatedTypeExpr {
    def annotations[A[_, _], K, L, I](e: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], K, L, I]): generic.TypeExpr[A, K, L, I] =
      e.translate[A]([k, l] => (ae: AnnotatedTypeExpr[A, k, l]) => ae.annotation)

    def annotate[A[_, _], K, L](
      f: TypeExpr[K, L],
      ann: [k, l] => generic.TypeExpr[A, k, l, ?] => A[k, l],
    ): AnnotatedTypeExpr[A, K, L] =
      annotateM[A, K, L, scalaz.Id.Id](f, ann)(using scalaz.Id.id)

    def annotateM[A[_, _], K, L, M[_]: Monad](
      f: TypeExpr[K, L],
      ann: [k, l] => generic.TypeExpr[A, k, l, ?] => M[A[k, l]],
    ): M[AnnotatedTypeExpr[A, K, L]] =
      f.foldM[AnnotatedTypeExpr[A, *, *], M](
        [k, l] => (tf: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], k, l, ?]) => {
          val ta: generic.TypeExpr[A, k, l, ?] = tf.translate[A](getAnnotation[A])
          ann(ta).map(AnnotatedTypeExpr(_, tf))
        }
      )

    private def getAnnotation[A[_, _]]: [x, y] => AnnotatedTypeExpr[A, x, y] => A[x, y] =
      [x, y] => (ate: AnnotatedTypeExpr[A, x, y]) => ate.annotation
  }

  case class TypeExpr[K, L](value: generic.TypeExpr[TypeExpr, K, L, ?]) {
    given inKind: Kind[K] = value.inKind
    given outKind: OutputKind[L] = value.outKind

    def foldM[F[_, _], M[_]: Monad](f: [k, l] => generic.TypeExpr[F, k, l, ?] => M[F[k, l]]): M[F[K, L]] = {
      value
        .translateM[F, M]([x, y] => (te: TypeExpr[x, y]) => te.foldM[F, M](f))
        .flatMap(f(_))
    }

    def ∘[J](that: TypeExpr[J, K]): TypeExpr[J, L] =
      that > this

    def >[M](that: TypeExpr[L, M]): TypeExpr[K, M] = {
      import generic.{TypeExpr => gt}

      TypeExpr(
        (this.value, that.value) match {
          case (f, gt.AppFst(g, b1)) =>
            gt.AppCompose(g, b1, TypeExpr(f))
          case (a, b) =>
            throw new NotImplementedError(s"$a > $b")
        }
      )
    }

    override def toString: String =
      value.toString
  }

  object TypeExpr {
    def unit: TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.UnitType())

    def int: TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.IntType())

    def string: TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.StringType())

    def inferenceVar(aliases: Set[Object]): TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.newInferenceVar())

    def appFst[K1, K2, L](
      op: generic.TypeExpr.BinaryOperator[?, K1, K2, L, ?],
      arg1: TypeExpr[○, K1],
    ): TypeExpr[K2, L] =
      TypeExpr(generic.TypeExpr.AppFst(op.cast[TypeExpr], arg1))

    def appCompose[K, L1, L2, M](
      op: generic.TypeExpr.BinaryOperator[?, L1, L2, M, ?],
      arg1: TypeExpr[○, L1],
      arg2: TypeExpr[K, L2],
    ): TypeExpr[K, M] = {
      import arg2.{given Kind[K]}
      TypeExpr(generic.TypeExpr.AppCompose(op.cast[TypeExpr], arg1, arg2))
    }

    def biApp[K1, K2, L](
      op: generic.TypeExpr.BinaryOperator[?, K1, K2, L, ?],
      arg1: TypeExpr[○, K1],
      arg2: TypeExpr[○, K2],
    ): TypeExpr[○, L] =
      TypeExpr(generic.TypeExpr.BiApp(op.cast[TypeExpr], arg1, arg2))

    def pair: TypeExpr[● × ●, ●] =
      TypeExpr(generic.TypeExpr.Pair())

    def pair(a: TypeExpr[○, ●], b: TypeExpr[○, ●]): TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.pair(a, b))

    def pair1(a: TypeExpr[○, ●]): TypeExpr[●, ●] =
      TypeExpr(generic.TypeExpr.pair1(a))

    def sum(a: TypeExpr[○, ●], b: TypeExpr[○, ●]): TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.sum(a, b))

    def sum1(a: TypeExpr[○, ●]): TypeExpr[●, ●] =
      TypeExpr(generic.TypeExpr.sum1(a))

    def fix[K](f: generic.Routing[●, K], g: TypeExpr[K, ●]): Type =
      TypeExpr(generic.TypeExpr.Fix(f, g))

    def pfix[K, X](f: generic.Routing[K × ●, X], g: TypeExpr[X, ●]): TypeExpr[K, ●] =
      TypeExpr(generic.TypeExpr.PFix(f, g))

    def scalaTypeParam[T](filename: String, line: Int, name: String): TypeExpr[○, ●] =
      TypeExpr(generic.TypeExpr.ScalaTypeParams.one(filename, line, name))

    def typeError[K: Kind, L: OutputKind](msg: String): TypeExpr[K, L] =
      TypeExpr(generic.TypeExpr.TypeError(msg))
  }

  type Type = TypeExpr[○, ●]
  object Type {
    def unit: Type   = TypeExpr.unit
    def int: Type    = TypeExpr.int
    def string: Type = TypeExpr.string

    def sum(a: Type, b: Type): Type =
      TypeExpr(generic.TypeExpr.sum(a, b))

    def fix(f: TypeFun[●, ●]): Type =
      TypeFun.toExpr(TypeFun.fix(f))
  }

  opaque type TypeTag[A <: AnyKind] = TypeFun[?, ?]
  object TypeTag {
    def apply[A <: AnyKind](using a: TypeTag[A]): TypeTag[A] =
      a

    given unit: TypeTag[Unit] = TypeFun.unit
    given int: TypeTag[Int] = TypeFun.int
    given string: TypeTag[String] = TypeFun.string

    given pair: TypeTag[Tuple2] =
      TypeFun.pair

    given pair[A, B](using a: TypeTag[A], b: TypeTag[B]): TypeTag[(A, B)] =
      TypeFun.pair(
        (a: TypeFun[?, ?]).asInstanceOf[TypeFun[○, ●]],
        (b: TypeFun[?, ?]).asInstanceOf[TypeFun[○, ●]],
      )

    given pair1[A](using a: TypeTag[A]): TypeTag[(A, *)] =
      TypeFun.pair1(
        (a: TypeFun[?, ?]).asInstanceOf[TypeFun[○, ●]]
      )

    given sum1[A](using a: TypeTag[A]): TypeTag[Either[A, *]] =
      TypeFun.sum1(
        (a: TypeFun[?, ?]).asInstanceOf[TypeFun[○, ●]]
      )

    given fix[F[_]](using f: TypeTag[F]): TypeTag[Fix[F]] =
      TypeFun.fix(
        (f: TypeFun[?, ?]).asInstanceOf[TypeFun[●, ●]]
      )

    given pfix[F[_, _]](using f: TypeTag[F]): TypeTag[[x] =>> Fix[F[x, *]]] =
      TypeFun.pfix(
        (f: TypeFun[?, ?]).asInstanceOf[TypeFun[● × ●, ●]]
      )

    def compose[F[_], G[_]](f: TypeTag[F], g: TypeTag[G]): TypeTag[[x] =>> F[G[x]]] = {
      val f1 = (f: TypeFun[?, ?]).asInstanceOf[TypeFun[●, ●]]
      val g1 = (g: TypeFun[?, ?]).asInstanceOf[TypeFun[●, ●]]
      f1 ∘ g1
    }

    def compose2[F[_], G[_, _]](f: TypeTag[F], g: TypeTag[G]): TypeTag[[x, y] =>> F[G[x, y]]] = {
      val f1 = (f: TypeFun[?, ?]).asInstanceOf[TypeFun[●, ●]]
      val g1 = (g: TypeFun[?, ?]).asInstanceOf[TypeFun[● × ●, ●]]
      f1 ∘ g1
    }

    def toType[A](ta: TypeTag[A]): Type =
      TypeFun.toExpr((ta: TypeFun[?, ?]).asInstanceOf[TypeFun[○, ●]])

    def toTypeFun[F[_]](tf: TypeTag[F]): TypeFun[●, ●] =
      (tf: TypeFun[?, ?]).asInstanceOf[TypeFun[●, ●]]

    import scala.{quoted => sq}
    private def fromTypeParam[T](using t: sq.Type[T], q: sq.Quotes): sq.Expr[TypeTag[T]] = {
      import q.reflect._

      val repr = TypeRepr.of[T]
      val pos = repr.typeSymbol.pos
      (repr, pos) match {
        case (_, None) =>
          sys.error(s"${sq.Type.show[T]} does not look like a type parameter, because it does not have a source position.")
        case (TypeRef(NoPrefix(), name), Some(pos)) =>
          val file = pos.sourceFile.path
          val line = pos.startLine
          '{
            TypeFun.scalaTypeParam[T](
              filename = ${sq.Expr(file)},
              line = ${sq.Expr(line)},
              name = ${sq.Expr(name)},
            )
          }
        case _ =>
          sys.error(repr.show + " does not look like a type parameter")
      }
    }

    inline def ofTypeParam[T]: TypeTag[T] =
      ${ fromTypeParam[T] }
  }

}
