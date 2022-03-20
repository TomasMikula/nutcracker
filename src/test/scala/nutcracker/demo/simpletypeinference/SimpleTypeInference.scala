package nutcracker.demo.simpletypeinference

import nutcracker.{IDom, IUpdateResult, Propagation}
import nutcracker.toolkit.PropagationToolkit
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.ast.Fun
import nutcracker.demo.simpletypeinference.ast.Fun._
import nutcracker.demo.simpletypeinference.types._

object SimpleTypeInference {
  def apply[F[_]](using P: Propagation[F]): SimpleTypeInference[F, P.type] =
    new SimpleTypeInference[F, P.type]

  def reconstructTypes[A, B](f: Fun[A, B]): (Tpe, Tpe) =
    PropagationToolkit.run { [F[_]] =>
      (propagation: Propagation[F]) =>
        SimpleTypeInference(using propagation).reconstructTypes(f)
    }
}

class SimpleTypeInference[F[_], Propagation <: nutcracker.Propagation[F]](using val P: Propagation) {
  import P.{ITrigger, IVar, M, Out, Var, iFire, iObserve, iOut, iReadOnly, iSleep, iUpdate, monadOut, newCell, readOnly}

  case class ITypeExpr[K, L, I](value: generic.TypeExpr[TypeExprCell, K, L, I])

  object ITypeExpr {
    def newInferenceVar(): ITypeExpr[○, ●, Tag.Var] =
      ITypeExpr(generic.TypeExpr.newInferenceVar())

    def biApp[K1, K2, L](
      op: generic.TypeExpr.BinaryOperator[TypeExprCell, K1, K2, L, ?],
      a1: TypeExprCell[○, K1],
      a2: TypeExprCell[○, K2],
    ): ITypeExpr[○, L, Tag.BiApp] =
      ITypeExpr(generic.TypeExpr.BiApp(op, a1, a2))

    type UpdRes[K, L, I0, J, I1] = generic.TypeExpr.UpdRes[TypeExprCell, K, L, I0, J, I1]
    type Change[K, L, I0, I1] = generic.TypeExpr.Change[TypeExprCell, K, L, I0, I1]

    implicit def dom[K, L]: IDom.Aux2[
      ITypeExpr[K, L, *],
      ITypeExpr[K, L, *],
      UpdRes[K, L, *, *, ?],
      Change[K, L, *, *],
    ] =
      new IDom[ITypeExpr[K, L, *]] {
        override type IUpdate[J] = ITypeExpr[K, L, J]
        override type IUpdateRes[I, J] = UpdRes[K, L, I, J, ?]
        override type IDelta[I0, I1] = Change[K, L, I0, I1]

        private val delegate = generic.TypeExpr.domTypeExpr[TypeExprCell, K, L]

        override def iUpdate[I, J](f: ITypeExpr[K, L, I], u: ITypeExpr[K, L, J]): IUpdateRes[I, J] =
          delegate.iUpdate[I, J](f.value, u.value)

        override def toUpdateResult[I, J](r: IUpdateRes[I, J]): IUpdateResult[ITypeExpr[K, L, *], IDelta, I, ?] =
          delegate.toUpdateResult(r).mapDomain([i] => (f: generic.TypeExpr[TypeExprCell, K, L, i]) => ITypeExpr(f))

        override def composeDeltas[I1, I2, I3](ε: IDelta[I2, I3], δ: IDelta[I1, I2]): IDelta[I1, I3] =
          delegate.composeDeltas(ε, δ)

        override def iIsFailed[I](f: ITypeExpr[K, L, I]): Boolean =
          delegate.iIsFailed(f.value)
      }
  }

  type ITpe[I] = ITypeExpr[○, ●, I]
  object ITpe {
    def newInferenceVar(): ITpe[Tag.Var] =
      ITypeExpr.newInferenceVar()

    def unit: ITpe[Tag.Unit] =
      ITypeExpr(generic.TypeExpr.UnitType())

    def int: ITpe[Tag.Int] =
      ITypeExpr(generic.TypeExpr.IntType())

    def string: ITpe[Tag.Str] =
      ITypeExpr(generic.TypeExpr.StringType())

    def pair(a: ITpe[?], b: ITpe[?]): F[ITpe[?]] =
      for {
        a <- TpeCell(a)
        b <- TpeCell(b)
      } yield ITypeExpr(generic.TypeExpr.pair[TypeExprCell](a, b))
  }

  import ITpe.newInferenceVar

  /** Has both the pure [[TypeExpr]] and its embedding into the propagation network. */
  type BiTypeExpr[K, L] = AnnotatedTypeExpr[TypeExprCell, K, L]
  object BiTypeExpr {
    def apply[K, L](cell: TypeExprCell[K, L], value: generic.TypeExpr[BiTypeExpr, K, L, ?]): BiTypeExpr[K, L] =
      AnnotatedTypeExpr(cell, value)

    def apply[K, L](value: generic.TypeExpr[BiTypeExpr, K, L, ?]): F[BiTypeExpr[K, L]] = {
      val expr: generic.TypeExpr[TypeExprCell, K, L, ?] = AnnotatedTypeExpr.annotations(value)
      TypeExprCell(ITypeExpr(expr)).map(BiTypeExpr(_, value))
    }

    def unroll[K, L](f: TypeExpr[K, L]): F[BiTypeExpr[K, L]] =
      AnnotatedTypeExpr.annotateM[TypeExprCell, K, L, F](
        f,
        [k, l] => (t: generic.TypeExpr[TypeExprCell, k, l, ?]) =>
          TypeExprCell(ITypeExpr(t))
      )
  }

  sealed trait TypeObject[K] {
    given kind: Kind[K]

    def map[L](f: generic.Route[K, L]): TypeObject[L] =
      f match {
        case generic.Route.Id() => this
      }

    def map[L](f: BiTypeExpr[K, L]): F[TypeObject[L]] = {
      import generic.{TypeExpr => gt}
      f.value match {
        case gt.AppFst(op, arg1) =>
          import op.{in2Kind, outKind}
          TypeExprCell
            .biApp(op.cast, arg1.annotation, TypeObject.toCell(this))
            .map(TypeObject(_))

        case gt.AppCompose(op, a, g) =>
          import op.{in2Kind, outKind}
          for {
            b <- map(g)
            cell <- TypeExprCell.biApp(op.cast, a.annotation, TypeObject.toCell(b))
          } yield TypeObject(cell)

        case gt.PFix(pre, expr) =>
          val a: ArgIntro[●, K × ●] = ArgIntro.introFst(this)
          a.pushThrough(pre) match {
            case ArgIntro.PushThroughRes(r, ai) =>
              for {
                expr1 <- ai.supplyTo(expr)
                cell0 <- TypeExprCell(expr1)
                cell1 <- TypeExprCell.fix(r, cell0)
              } yield TypeObject(cell1)(using Kind.Type)
          }

        case other =>
          throw new NotImplementedError(s"$other")
      }
    }
  }

  object TypeObject {
    case class WrapCell[K: Kind](cell: TypeExprCell[○, K]) extends TypeObject[K] {
      override def kind: Kind[K] = summon
    }

    def apply[K: Kind](cell: TypeExprCell[○, K]): TypeObject[K] =
      WrapCell(cell)

    def toCell[K: OutputKind](o: TypeObject[K]): TypeExprCell[○, K] =
      o match {
        case WrapCell(cell) => cell
      }
  }

  /** Introduces type arguments into `K`, obtaining `L`. */
  sealed trait ArgIntro[K, L] {
    import ArgIntro._

    given inKind: Kind[K]
    given outKind: Kind[L]

    def pushThrough[M](r: generic.Route[L, M]): PushThroughRes[K, ?, M] =
      r match {
        case generic.Route.Id() =>
          PushThroughRes(generic.Route.Id(), this)
      }

    def supplyTo[M](e: BiTypeExpr[L, M]): F[ITypeExpr[K, M, ?]] = {
        import generic.{TypeExpr => gt}

        e.value match {
          case gt.AppCompose(op, a, g) =>
            for {
              h <- this.supplyTo(g)
              hc <- TypeExprCell(h)
            } yield
              ITypeExpr(gt.AppCompose(op.cast, a.annotation, hc))

          case gt.Pair() =>
            this match {
              case IntroFst(a) => ITypeExpr(gt.pair1(TypeObject.toCell(a))).pure[F]
              case other => throw new NotImplementedError(s"$other")
            }

          case other =>
            throw new NotImplementedError(s"$other")
        }
      }
  }
  object ArgIntro {
    case class IntroFst[K: Kind, L: Kind](arg: TypeObject[K]) extends ArgIntro[L, K × L] {
      override def inKind: Kind[L] = summon[Kind[L]]
      override def outKind: Kind[K × L] = summon[Kind[K × L]]
    }

    case class PushThroughRes[K, X, L](r: generic.Route[K, X], ai: ArgIntro[X, L])

    def introFst[K: Kind, L: Kind](a: TypeObject[K]): ArgIntro[L, K × L] =
      IntroFst(a)
  }

  type TypeExprCell[K, L] = IVar[ITypeExpr[K, L, *]]
  object TypeExprCell {
    def apply[K, L, I](t: ITypeExpr[K, L, I]): F[TypeExprCell[K, L]] =
      P.newICell(t)

    def biApp[K1, K2, L](
      op: generic.TypeExpr.BinaryOperator[TypeExprCell, K1, K2, L, ?],
      a1: TypeExprCell[○, K1],
      a2: TypeExprCell[○, K2],
    ): F[TypeExprCell[○, L]] =
      TypeExprCell(ITypeExpr.biApp(op, a1, a2))

    def fix[K](f: generic.Route[●, K], g: TypeExprCell[K, ●]): F[TypeExprCell[○, ●]] =
      TypeExprCell(ITypeExpr(generic.TypeExpr.Fix(f, g)))
  }

  type TpeCell = TypeExprCell[○, ●]
  object TpeCell {
    def apply[I](t: ITpe[I]): F[TpeCell] =
      P.newICell(t)

    def pair(a: TpeCell, b: TpeCell): F[TpeCell] =
      TpeCell(ITypeExpr(generic.TypeExpr.pair(a, b)))

    def sum(a: TpeCell, b: TpeCell): F[TpeCell] =
      TpeCell(ITypeExpr(generic.TypeExpr.sum(a, b)))

    def fix[K](f: generic.Route[●, K], g: TypeExprCell[K, ●]): F[TpeCell] =
      TypeExprCell.fix(f, g)
  }

  def newTpeCell[I](t: ITpe[I]): F[TpeCell] =
    P.newICell(t)

  def reconstructTypes[A, B](
    f: Fun[A, B]
  ): F[Out[(Tpe, Tpe)]] =
    for {
      ta <- newTpeCell(newInferenceVar())
      tb <- newTpeCell(newInferenceVar())
      _ <- connect(ta, f, tb, Map.empty)
    } yield for {
      ta <- outputTpeCell(ta)
      tb <- outputTpeCell(tb)
    } yield (ta, tb)

  private def connect[A, B](
    a: TpeCell,
    f: Fun[A, B],
    b: TpeCell,
    ctx: Map[Fun.Label[?, ?], (TpeCell, TpeCell)],
  ): F[Unit] =
    f match {
      case IdFun() =>
        unifyTpes(a, b)

      case AndThen(g, h) =>
        def go[X](g: Fun[A, X], h: Fun[X, B]): F[Unit] =
          newTpeCell(newInferenceVar())
            .flatMap { x => connect(a, g, x, ctx) *> connect(x, h, b, ctx) }
        go(g, h)

      case par: Par[a1, a2, b1, b2] =>
        def go[A1, A2, B1, B2](f1: Fun[A1, B1], f2: Fun[A2, B2]): F[Unit] =
          for {
            a1 <- newTpeCell(newInferenceVar())
            a2 <- newTpeCell(newInferenceVar())
            b1 <- newTpeCell(newInferenceVar())
            b2 <- newTpeCell(newInferenceVar())
            a12 <- TpeCell.pair(a1, a2)
            b12 <- TpeCell.pair(b1, b2)
            _ <- unifyTpes(a, a12)
            _ <- unifyTpes(b, b12)
            _ <- connect(a1, f1, b1, ctx)
            _ <- connect(a2, f2, b2, ctx)
          } yield ()
        go[a1, a2, b1, b2](par.f1, par.f2)

      case e: EitherF[a1, a2, B] =>
        def go[A1, A2](g: Fun[A1, B], h: Fun[A2, B]): F[Unit] =
          for {
            a1  <- newTpeCell(newInferenceVar())
            a2  <- newTpeCell(newInferenceVar())
            a12 <- TpeCell.sum(a1, a2)
            _   <- unifyTpes(a, a12)
            _   <- connect(a1, g, b, ctx)
            _   <- connect(a2, h, b, ctx)
          } yield ()
        go[a1, a2](e.f, e.g)

      case i: InjectL[A, b2] =>
        for {
          b2  <- newTpeCell(newInferenceVar())
          ab2 <- TpeCell.sum(a, b2)
          _   <- unifyTpes(b, ab2)
        } yield ()

      case i: InjectR[A, b1] =>
        for {
          b1  <- newTpeCell(newInferenceVar())
          b1a <- TpeCell.sum(b1, a)
          _   <- unifyTpes(b, b1a)
        } yield ()

      case fix: FixF[g] =>
        val tf = TypeTag.toTpeFun(fix.f)
        for {
          g <- BiTypeExpr.unroll(tf.expr)
          fg <- TpeCell.fix(tf.pre, g.annotation)
          gfg <- TypeObject(fg).map(tf.pre).map(g).map(TypeObject.toCell)
          _ <- unifyTpes(a, gfg)
          _ <- unifyTpes(b, fg)
        } yield ()

      case unfix: UnfixF[g] =>
        val tf = TypeTag.toTpeFun(unfix.f)
        for {
          g <- BiTypeExpr.unroll(tf.expr)
          fg <- TpeCell.fix(tf.pre, g.annotation)
          gfg <- TypeObject(fg).map(tf.pre).map(g).map(TypeObject.toCell)
          _  <- unifyTpes(a, fg)
          _  <- unifyTpes(b, gfg)
        } yield ()

      case Rec(label, f) =>
        connect(a, f, b, ctx.updated(label, (a, b)))

      case RecCall(label) =>
        ctx.get(label) match {
          case Some((ta, tb)) => unifyTpes(a, ta) *> unifyTpes(b, tb)
          case None           => throw new IllegalArgumentException("Recursive call to an undefined function")
        }

      case ConstInt(_) =>
        for {
          _ <- refinePropagate(a, ITpe.unit)
          _ <- refinePropagate(b, ITpe.int)
        } yield ()

      case AddInts() =>
        for {
          ii <- ITpe.pair(ITpe.int, ITpe.int)
          _ <- refinePropagate(a, ii)
          _ <- refinePropagate(b, ITpe.int)
        } yield ()

      case IntToString() =>
        for {
          _ <- refinePropagate(a, ITpe.int)
          _ <- refinePropagate(b, ITpe.string)
        } yield ()
    }

  private def outputTpeCell(tc: TpeCell): Out[Tpe] =
    outputTypeExprCell(tc)

  private def outputTypeExprCell[K, L](tc: TypeExprCell[K, L]): Out[TypeExpr[K, L]] =
    iOut(tc).flatMap(t => outputTpe(t.value))

  private def outputTpe[K, L, I](
    t: ITypeExpr[K, L, I],
  ): Out[TypeExpr[K, L]] = {
    import generic.{TypeExpr => gte}
    import t.value.{inKind, outKind}

    t.value match {
      case gte.InferenceVar(aliases)  => TypeExpr.inferenceVar(aliases).pure[Out]
      case gte.AppFst(op, a1)         => outputTypeExprCell(a1).map(TypeExpr.appFst(op, _))
      case gte.AppCompose(op, a1, f2) => (outputTypeExprCell(a1) |@| outputTypeExprCell(f2)) { (a1, f2) => TypeExpr.appCompose(op, a1, f2) }
      case gte.BiApp(op, a1, a2)      => (outputTypeExprCell(a1) |@| outputTypeExprCell(a2)) { (a1, a2) => TypeExpr.biApp(op, a1, a2) }
      case gte.UnitType()             => TypeExpr.unit.pure[Out]
      case gte.IntType()              => TypeExpr.int.pure[Out]
      case gte.StringType()           => TypeExpr.string.pure[Out]
      case gte.Pair()                 => TypeExpr.pair.pure[Out]
      case gte.Fix(f, g)              => outputTypeExprCell(g).map(TypeExpr.fix(f, _))
      case gte.PFix(f, g)             => outputTypeExprCell(g).map(TypeExpr.pfix(f, _))
      case gte.TypeError(msg)         => TypeExpr.typeError(msg).pure[Out]
      case other                      => throw new NotImplementedError(s"$other")
    }
  }

  private def unifyTpes(
    t1: TpeCell,
    t2: TpeCell,
  ): F[Unit] =
    propagateTpe(t1, t2) *> propagateTpe(t2, t1)

  private def propagateTpe[K, L](
    src: TypeExprCell[K, L],
    tgt: TypeExprCell[K, L],
  ): F[Unit] =
    if (src == tgt) { // TODO: use Equal typeclass on cells (add it to Propagation)
      ().pure[F]
    } else {
      iObserve(iReadOnly(src)).by_ {
        P.iContinually[ITypeExpr[K, L, *], ITypeExpr.Change[K, L, *, *]] {
          [i] => (t: ITypeExpr[K, L, i]) => refinePropagate[K, L, i](tgt, t)
        }
      }
    }

  private def refinePropagate[K, L, J](
    cell: TypeExprCell[K, L],
    update: ITypeExpr[K, L, J],
  ): F[Unit] = {
    def handleResult[I0, I1](
      update: ITypeExpr[K, L, J],
      updRes: ITypeExpr.UpdRes[K, L, I0, J, I1],
    ): F[Unit] = {
      import generic.TypeExpr.UpdRes._

      updRes match {
        case UpdatedAliases(_, _)                 => M.pure(())
        case AlreadySuperset(_)                   => M.pure(())
        case AlreadyRefined(_)                    => M.pure(())
        case AlreadyConcrete(_)                   => M.pure(())
        case AlreadySameAtom()                    => M.pure(())
        case SubstitutedForVar(_)                 => M.pure(())
        case SubstitutedForParams(_)              => M.pure(())
        case AlreadyAppFst(_, a1, u1)             => propagateTpe(u1, a1)
        case AlreadyBiApp(_, a1, a2, u1, u2)      => propagateTpe(u1, a1) *> propagateTpe(u2, a2)
        case AlreadyAppCompose(_, a1, a2, u1, u2) => propagateTpe(u1, a1) *> propagateTpe(u2, a2)
        case AlreadyFix(_, t, u)                  => propagateTpe(u, t)
        case AlreadyPFix(_, t, u)                 => propagateTpe(u, t)
        case Failed(_)                            => M.pure(())
        // case AlreadyFailed(_)                     => M.pure(())
        case other => throw new NotImplementedError(s"$other")
      }
    }

    for {
      updRes <- iUpdate(cell)(update)
      _      <- handleResult(update, updRes.value)
    } yield ()
  }
}
