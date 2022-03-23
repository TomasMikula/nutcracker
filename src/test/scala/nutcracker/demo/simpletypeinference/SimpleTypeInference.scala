package nutcracker.demo.simpletypeinference

import nutcracker.{IDom, IUpdateResult, Propagation}
import nutcracker.toolkit.PropagationToolkit
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.ast.Fun
import nutcracker.demo.simpletypeinference.ast.Fun._
import nutcracker.demo.simpletypeinference.kinds._
import nutcracker.demo.simpletypeinference.types._
import nutcracker.demo.simpletypeinference.types.generic.TypeExpr.Tag

object SimpleTypeInference {
  def apply[F[_]](using P: Propagation[F]): SimpleTypeInference[F, P.type] =
    new SimpleTypeInference[F, P.type]

  def reconstructTypes[A, B](f: Fun[A, B]): (Type, Type) =
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

    def fix[X](f: Routing[●, X], g: TypeExprCell[X, ●]): ITypeExpr[○, ●, Tag.Fix] =
      ITypeExpr(generic.TypeExpr.Fix(f, g))

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

  type IType[I] = ITypeExpr[○, ●, I]
  object IType {
    def newInferenceVar(): IType[Tag.Var] =
      ITypeExpr.newInferenceVar()

    def unit: IType[Tag.Unit] =
      ITypeExpr(generic.TypeExpr.UnitType())

    def int: IType[Tag.Int] =
      ITypeExpr(generic.TypeExpr.IntType())

    def string: IType[Tag.Str] =
      ITypeExpr(generic.TypeExpr.StringType())

    def pair(a: IType[?], b: IType[?]): F[IType[?]] =
      for {
        a <- TypeCell(a)
        b <- TypeCell(b)
      } yield ITypeExpr(generic.TypeExpr.pair[TypeExprCell](a, b))
  }

  import IType.newInferenceVar

  /** Has both the pure [[TypeExpr]] and its embedding into the propagation network. */
  type CellAnnotatedTypeExpr[K, L] = AnnotatedTypeExpr[TypeExprCell, K, L]
  object CellAnnotatedTypeExpr {
    def apply[K, L](
      cell: TypeExprCell[K, L],
      value: generic.TypeExpr[CellAnnotatedTypeExpr, K, L, ?],
    ): CellAnnotatedTypeExpr[K, L] =
      AnnotatedTypeExpr(cell, value)

    def apply[K, L](value: generic.TypeExpr[CellAnnotatedTypeExpr, K, L, ?]): F[CellAnnotatedTypeExpr[K, L]] = {
      val expr: generic.TypeExpr[TypeExprCell, K, L, ?] = AnnotatedTypeExpr.annotations(value)
      TypeExprCell(ITypeExpr(expr)).map(CellAnnotatedTypeExpr(_, value))
    }

    def unroll[K, L](f: TypeExpr[K, L]): F[CellAnnotatedTypeExpr[K, L]] =
      AnnotatedTypeExpr.annotateM[TypeExprCell, K, L, F](
        f,
        [k, l] => (t: generic.TypeExpr[TypeExprCell, k, l, ?]) =>
          TypeExprCell(ITypeExpr(t))
      )
  }

  type CellIntro[K, L] = ArgIntro[TypeExprCell[○, *], K, L]
  object CellIntro {
    def apply[K: ProperKind](cell: TypeExprCell[○, K]): CellIntro[○, K] =
      ArgIntro.wrapArg(cell)

    def introFst[K, L: ProperKind](a: CellIntro[○, K]): CellIntro[L, K × L] =
      ArgIntro.introFst(a)

    def toCell[K: OutputKind](ci: CellIntro[○, K]): TypeExprCell[○, K] =
      ArgIntro.unwrap(ci)

    def supplyTo[K, L, M](a: CellIntro[K, L], e: CellAnnotatedTypeExpr[L, M]): F[ITypeExpr[K, M, ?]] =
      a.inKind.properKind match {
        case Left(k_eq_○) =>
          k_eq_○.substituteContra[[k] =>> F[ITypeExpr[k, M, ?]]](
            CellIntro.supplyTo0(k_eq_○.substituteCo[CellIntro[*, L]](a), e)
          )
        case Right(k) =>
          CellIntro.supplyTo1(a, e)(using k)
      }

    private def supplyTo0[L, M](args: CellIntro[○, L], e: CellAnnotatedTypeExpr[L, M]): F[ITypeExpr[○, M, ?]] ={
      import generic.{TypeExpr => gt}

      e.value match {
        case gt.AppFst(op, arg1) =>
          import op.in2Kind
          ITypeExpr
            .biApp(op.cast, arg1.annotation, toCell(args))
            .pure[F]

        case gt.AppCompose(op, a, g) =>
          for {
            b <- supplyTo0(args, g)
            bc <- TypeExprCell(b)
          } yield ITypeExpr.biApp(op.cast, a.annotation, bc)

        case gt.PFix(pre, expr) =>
          val a: CellIntro[●, L × ●] = introFst(args)
          pre.applyTo(a) match {
            case Routing.ApplyRes(r, ai) =>
              for {
                expr1 <- supplyTo(ai, expr)
                cell0 <- TypeExprCell(expr1)
              } yield ITypeExpr.fix(r, cell0)
          }

        case other =>
          throw new NotImplementedError(s"Supplying $args into $other")
      }
    }

    private def supplyTo1[K: ProperKind, L, M](args: CellIntro[K, L], e: CellAnnotatedTypeExpr[L, M]): F[ITypeExpr[K, M, ?]] = {
      import generic.{TypeExpr => gt}

      e.value match {
        case gt.AppCompose(op, a, g) =>
          for {
            h <- supplyTo1(args, g)
            hc <- TypeExprCell(h)
          } yield
            ITypeExpr(gt.AppCompose(op.cast, a.annotation, hc))

        case gt.Pair() =>
          args match {
            case ArgIntro.IntroFst(a) => ITypeExpr(gt.pair1(toCell(a))).pure[F]
            case other => throw new NotImplementedError(s"$other")
          }

        case other =>
          throw new NotImplementedError(s"$other")
      }
    }
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

    def fix[K](f: Routing[●, K], g: TypeExprCell[K, ●]): F[TypeExprCell[○, ●]] =
      TypeExprCell(ITypeExpr.fix(f, g))
  }

  type TypeCell = TypeExprCell[○, ●]
  object TypeCell {
    def apply[I](t: IType[I]): F[TypeCell] =
      P.newICell(t)

    def pair(a: TypeCell, b: TypeCell): F[TypeCell] =
      TypeCell(ITypeExpr(generic.TypeExpr.pair(a, b)))

    def sum(a: TypeCell, b: TypeCell): F[TypeCell] =
      TypeCell(ITypeExpr(generic.TypeExpr.sum(a, b)))

    def fix[K](f: Routing[●, K], g: TypeExprCell[K, ●]): F[TypeCell] =
      TypeExprCell.fix(f, g)
  }

  def newTypeCell[I](t: IType[I]): F[TypeCell] =
    P.newICell(t)

  def reconstructTypes[A, B](
    f: Fun[A, B]
  ): F[Out[(Type, Type)]] =
    for {
      ta <- newTypeCell(newInferenceVar())
      tb <- newTypeCell(newInferenceVar())
      _ <- connect(ta, f, tb, Map.empty)
    } yield for {
      ta <- outputTypeCell(ta)
      tb <- outputTypeCell(tb)
    } yield (ta, tb)

  private def connect[A, B](
    a: TypeCell,
    f: Fun[A, B],
    b: TypeCell,
    ctx: Map[Fun.Label[?, ?], (TypeCell, TypeCell)],
  ): F[Unit] =
    f match {
      case IdFun() =>
        unifyTypes(a, b)

      case AndThen(g, h) =>
        def go[X](g: Fun[A, X], h: Fun[X, B]): F[Unit] =
          newTypeCell(newInferenceVar())
            .flatMap { x => connect(a, g, x, ctx) *> connect(x, h, b, ctx) }
        go(g, h)

      case par: Par[a1, a2, b1, b2] =>
        def go[A1, A2, B1, B2](f1: Fun[A1, B1], f2: Fun[A2, B2]): F[Unit] =
          for {
            a1 <- newTypeCell(newInferenceVar())
            a2 <- newTypeCell(newInferenceVar())
            b1 <- newTypeCell(newInferenceVar())
            b2 <- newTypeCell(newInferenceVar())
            a12 <- TypeCell.pair(a1, a2)
            b12 <- TypeCell.pair(b1, b2)
            _ <- unifyTypes(a, a12)
            _ <- unifyTypes(b, b12)
            _ <- connect(a1, f1, b1, ctx)
            _ <- connect(a2, f2, b2, ctx)
          } yield ()
        go[a1, a2, b1, b2](par.f1, par.f2)

      case e: EitherF[a1, a2, B] =>
        def go[A1, A2](g: Fun[A1, B], h: Fun[A2, B]): F[Unit] =
          for {
            a1  <- newTypeCell(newInferenceVar())
            a2  <- newTypeCell(newInferenceVar())
            a12 <- TypeCell.sum(a1, a2)
            _   <- unifyTypes(a, a12)
            _   <- connect(a1, g, b, ctx)
            _   <- connect(a2, h, b, ctx)
          } yield ()
        go[a1, a2](e.f, e.g)

      case i: InjectL[A, b2] =>
        for {
          b2  <- newTypeCell(newInferenceVar())
          ab2 <- TypeCell.sum(a, b2)
          _   <- unifyTypes(b, ab2)
        } yield ()

      case i: InjectR[A, b1] =>
        for {
          b1  <- newTypeCell(newInferenceVar())
          b1a <- TypeCell.sum(b1, a)
          _   <- unifyTypes(b, b1a)
        } yield ()

      case fix: FixF[g] =>
        val tf = TypeTag.toTypeFun(fix.f)
        for {
          g <- CellAnnotatedTypeExpr.unroll(tf.expr)
          fg <- TypeCell.fix(tf.pre, g.annotation)
          gfg <- CellIntro.supplyTo(tf.pre.applyTo0(CellIntro(fg)), g).flatMap(TypeExprCell(_))
          _ <- unifyTypes(a, gfg)
          _ <- unifyTypes(b, fg)
        } yield ()

      case unfix: UnfixF[g] =>
        val tf = TypeTag.toTypeFun(unfix.f)
        for {
          g <- CellAnnotatedTypeExpr.unroll(tf.expr)
          fg <- TypeCell.fix(tf.pre, g.annotation)
          gfg <- CellIntro.supplyTo(tf.pre.applyTo0(CellIntro(fg)), g).flatMap(TypeExprCell(_))
          _  <- unifyTypes(a, fg)
          _  <- unifyTypes(b, gfg)
        } yield ()

      case Rec(label, f) =>
        connect(a, f, b, ctx.updated(label, (a, b)))

      case RecCall(label) =>
        ctx.get(label) match {
          case Some((ta, tb)) => unifyTypes(a, ta) *> unifyTypes(b, tb)
          case None           => throw new IllegalArgumentException("Recursive call to an undefined function")
        }

      case ConstInt(_) =>
        for {
          _ <- refinePropagate(a, IType.unit)
          _ <- refinePropagate(b, IType.int)
        } yield ()

      case AddInts() =>
        for {
          ii <- IType.pair(IType.int, IType.int)
          _ <- refinePropagate(a, ii)
          _ <- refinePropagate(b, IType.int)
        } yield ()

      case IntToString() =>
        for {
          _ <- refinePropagate(a, IType.int)
          _ <- refinePropagate(b, IType.string)
        } yield ()
    }

  private def outputTypeCell(tc: TypeCell): Out[Type] =
    outputTypeExprCell(tc)

  private def outputTypeExprCell[K, L](tc: TypeExprCell[K, L]): Out[TypeExpr[K, L]] =
    iOut(tc).flatMap(t => outputType(t.value))

  private def outputType[K, L, I](
    t: ITypeExpr[K, L, I],
  ): Out[TypeExpr[K, L]] = {
    import generic.{TypeExpr => gte}
    import t.value.{inKind, outKind}

    t.value match {
      case gte.UnitType()            => TypeExpr.unit.pure[Out]
      case gte.IntType()             => TypeExpr.int.pure[Out]
      case gte.StringType()          => TypeExpr.string.pure[Out]
      case gte.Pair()                => TypeExpr.pair.pure[Out]
      case gte.InferenceVar(aliases) => TypeExpr.inferenceVar(aliases).pure[Out]
      case gte.TypeError(msg)        => TypeExpr.typeError(msg).pure[Out]
      case gte.AppFst(op, a1) =>
        outputTypeExprCell(a1).map(TypeExpr.appFst(op, _))
      case ac @ gte.AppCompose(op, a1, f2) =>
        (outputTypeExprCell(a1) |@| outputTypeExprCell(f2)) { (a1, f2) =>
          TypeExpr.appCompose(op, a1, f2)(using ac.properInKind)
        }
      case gte.BiApp(op, a1, a2) =>
        (outputTypeExprCell(a1) |@| outputTypeExprCell(a2)) { (a1, a2) =>
          TypeExpr.biApp(op, a1, a2)
        }
      case gte.Fix(f, g) =>
        outputTypeExprCell(g).map(TypeExpr.fix(f, _))
      case pf @ gte.PFix(f, g) =>
        outputTypeExprCell(g).map(TypeExpr.pfix(f, _)(using pf.properInKind))
      case other =>
        throw new NotImplementedError(s"$other")
    }
  }

  private def unifyTypes(
    t1: TypeCell,
    t2: TypeCell,
  ): F[Unit] =
    propagateType(t1, t2) *> propagateType(t2, t1)

  private def propagateType[K, L](
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
        case AlreadyAppFst(_, a1, u1)             => propagateType(u1, a1)
        case AlreadyBiApp(_, a1, a2, u1, u2)      => propagateType(u1, a1) *> propagateType(u2, a2)
        case AlreadyAppCompose(_, a1, a2, u1, u2) => propagateType(u1, a1) *> propagateType(u2, a2)
        case AlreadyFix(_, t, u)                  => propagateType(u, t)
        case AlreadyPFix(_, t, u)                 => propagateType(u, t)
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
