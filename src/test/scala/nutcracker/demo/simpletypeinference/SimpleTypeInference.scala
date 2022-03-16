package nutcracker.demo.simpletypeinference

import nutcracker.{IDom, IUpdateResult, Propagation}
import nutcracker.toolkit.PropagationToolkit
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.ast.Fun
import nutcracker.demo.simpletypeinference.ast.Fun._
import nutcracker.demo.simpletypeinference.types._
import nutcracker.data.PromiseOnce

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

    def lift[K, L](te: TypeExpr[K, L]): F[ITypeExpr[K, L, ?]] =
      ???

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

  case class ITypeFun[K, L, I](value: generic.TypeFun[TypeFunCell, K, L, I])

  object ITypeFun {
    def andThen[K: Kind, L: Kind, M: Kind](
      f: TypeFunCell[K, L],
      g: TypeFunCell[L, M],
    ): ITypeFun[K, M, Tag.Comp] =
      ITypeFun(generic.TypeFun.AndThen(f, g))

    def id[K: Kind]: ITypeFun[K, K, Tag.Id] =
      ITypeFun(generic.TypeFun.Id())

    def pair: ITypeFun[● × ●, ●, Tag.Prod] =
      ITypeFun(generic.TypeFun.pair)

    def sum: ITypeFun[● × ●, ●, Tag.Sum] =
      ITypeFun(generic.TypeFun.sum)

    def par[K1: Kind, K2: Kind, L1: Kind, L2: Kind](
      f1: TypeFunCell[K1, L1],
      f2: TypeFunCell[K2, L2],
    ): ITypeFun[K1 × K2, L1 × L2, Tag.Par] =
      ITypeFun(generic.TypeFun.Par(f1, f2))

    def introFst[K: Kind]: ITypeFun[K, ○ × K, Tag.IFst] =
      ITypeFun(generic.TypeFun.IntroFst())

    def dup[K: Kind]: ITypeFun[K, K × K, Tag.Dup] =
      ITypeFun(generic.TypeFun.Dup())

    def fix(f: TypeFunCell[●, ●]): ITypeFun[○, ●, Tag.Fix] =
      ITypeFun(generic.TypeFun.Fix(f))

    def newVar[K: Kind, L: Kind](): ITypeFun[K, L, Tag.Var] =
      ITypeFun(generic.TypeFun.newVar())

    def typeError[K: Kind, L: Kind](msg: String): ITypeFun[K, L, Tag.Err] =
      ITypeFun(generic.TypeFun.TypeError(msg))

    type UpdRes[K, L, I0, J, I1] = generic.TypeFun.UpdRes[TypeFunCell, K, L, I0, J, I1]
    type Change[K, L, I0, I1] = generic.TypeFun.Change[TypeFunCell, K, L, I0, I1]

    implicit def dom[K, L]: IDom.Aux2[
      ITypeFun[K, L, *],
      ITypeFun[K, L, *],
      UpdRes[K, L, *, *, ?],
      Change[K, L, *, *],
    ] =
      new IDom[ITypeFun[K, L, *]] {
        override type IUpdate[J] = ITypeFun[K, L, J]
        override type IUpdateRes[I, J] = UpdRes[K, L, I, J, ?]
        override type IDelta[I0, I1] = Change[K, L, I0, I1]

        private val delegate = generic.TypeFun.domTypeFun[TypeFunCell, K, L]

        override def iUpdate[I, J](f: ITypeFun[K, L, I], u: ITypeFun[K, L, J]): IUpdateRes[I, J] =
          delegate.iUpdate[I, J](f.value, u.value)

        override def toUpdateResult[I, J](r: IUpdateRes[I, J]): IUpdateResult[ITypeFun[K, L, *], IDelta, I, ?] =
          delegate.toUpdateResult(r).mapDomain([i] => (f: generic.TypeFun[TypeFunCell, K, L, i]) => ITypeFun(f))

        override def composeDeltas[I1, I2, I3](ε: IDelta[I2, I3], δ: IDelta[I1, I2]): IDelta[I1, I3] =
          delegate.composeDeltas(ε, δ)

        override def iIsFailed[I](f: ITypeFun[K, L, I]): Boolean =
          delegate.iIsFailed(f.value)
      }
  }

  type IType[I] = ITypeFun[○, ●, I]
  object IType {
    val Unit   : IType[Tag.Unit] = ITypeFun(generic.TypeFun.UnitType())
    val Int    : IType[Tag.Int]  = ITypeFun(generic.TypeFun.IntType())
    val String : IType[Tag.Str]  = ITypeFun(generic.TypeFun.StringType())
  }

  type IType1[I] = ITypeFun[●, ●, I]
  object IType1 {
    def typeError(msg: String): IType1[Tag.Err] =
      ITypeFun(generic.TypeFun.TypeError(msg))
  }

  /** Has both the pure [[TypeExpr]] and its embedding into the propagation network. */
  type BiTypeExpr[K, L] = AnnotatedTypeExpr[TypeExprCell, K, L]
  object BiTypeExpr {
    def unroll[K, L](f: TypeExpr[K, L]): F[BiTypeExpr[K, L]] =
      AnnotatedTypeExpr.annotateM[TypeExprCell, K, L, F](
        f,
        [k, l] => (t: generic.TypeExpr[TypeExprCell, k, l, ?]) =>
          TypeExprCell(ITypeExpr(t))
      )
  }

  sealed trait TypeObject[K] {
    def map[L](f: generic.Route[K, L]): TypeObject[L] =
      f match {
        case generic.Route.Id() => this
      }

    def map[L](f: BiTypeExpr[K, L]): F[TypeObject[L]] = {
      import generic.{TypeExpr => gt}
      f.value match {
        case ap: gt.AppFst[BiTypeExpr, k0, K, L] =>
          import ap.op.in2Kind
          TypeExprCell
            .biApp(ap.op.cast, ap.arg1.annotation, TypeObject.toCell(this))
            .map(TypeObject(_))

        case other => throw new NotImplementedError(s"$other")
      }
    }
  }

  object TypeObject {
    case class WrapCell[K](cell: TypeExprCell[○, K]) extends TypeObject[K]

    def apply[K](cell: TypeExprCell[○, K]): TypeObject[K] =
      WrapCell(cell)

    def toCell[K: OutputKind](o: TypeObject[K]): TypeExprCell[○, K] =
      o match {
        case WrapCell(cell) => cell
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
      TpeCell(ITypeExpr(generic.TypeExpr.Fix(f, g)))
  }

  type TypeFunCell[K, L] = IVar[ITypeFun[K, L, *]]
  object TypeFunCell {
    def apply[K, L, I](f: ITypeFun[K, L, I]): F[TypeFunCell[K, L]] =
      P.newICell(f)

    def andThen[K: Kind, L: Kind, M: Kind](f: TypeFunCell[K, L], g: TypeFunCell[L, M]): F[TypeFunCell[K, M]] =
      TypeFunCell(ITypeFun.andThen(f, g))

    def pair: F[TypeFunCell[● × ●, ●]] =
      TypeFunCell(ITypeFun.pair)

    def sum: F[TypeFunCell[● × ●, ●]] =
      TypeFunCell(ITypeFun.sum)

    def par[K1: Kind, K2: Kind, L1: Kind, L2: Kind](
      f1: TypeFunCell[K1, L1],
      f2: TypeFunCell[K2, L2],
    ): F[TypeFunCell[K1 × K2, L1 × L2]] =
      TypeFunCell(ITypeFun.par(f1, f2))

    def dup[K: Kind]: F[TypeFunCell[K, K × K]] =
      TypeFunCell(ITypeFun.dup[K])

    def fix(f: TypeFunCell[●, ●]): F[TypeFunCell[○, ●]] =
      TypeFunCell(ITypeFun.fix(f))
  }

  extension [K: Kind, L: Kind](f: F[TypeFunCell[K, L]]) {
    def >=>[M: Kind](g: F[TypeFunCell[L, M]]): F[TypeFunCell[K, M]] =
      for {
        f <- f
        g <- g
        h <- TypeFunCell.andThen(f, g)
      } yield h
  }

  type TypeCell = TypeFunCell[○, ●]
  object TypeCell {
    import TypeFunCell.{dup, par}

    def pair(a: TypeCell, b: TypeCell): F[TypeCell] =
      dup[○] >=> par(a, b) >=> TypeFunCell.pair

    def sum(a: TypeCell, b: TypeCell): F[TypeCell] =
      dup[○] >=> par(a, b) >=> TypeFunCell.sum

    def fix(f: Type1Cell): F[TypeCell] =
      TypeFunCell.fix(f)
  }

  type Type1Cell = TypeFunCell[●, ●]

  def newTypeVar(): IType[Tag.Var] =
    ITypeFun.newVar[○, ●]()

  def newType1Var(): IType1[Tag.Var] =
    ITypeFun.newVar[●, ●]()

  def newTypeCell[I](t: IType[I]): F[TypeCell] =
    P.newICell(t)

  def newType1Cell[I](t: IType1[I]): F[Type1Cell] =
    P.newICell(t)

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

  private def connectar[A, B](
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
          newTypeCell(newTypeVar())
            .flatMap { x => connectar(a, g, x, ctx) *> connectar(x, h, b, ctx) }
        go(g, h)

      case par: Par[a1, a2, b1, b2] =>
        def go[A1, A2, B1, B2](f1: Fun[A1, B1], f2: Fun[A2, B2]): F[Unit] =
          for {
            a1 <- newTypeCell(newTypeVar())
            a2 <- newTypeCell(newTypeVar())
            b1 <- newTypeCell(newTypeVar())
            b2 <- newTypeCell(newTypeVar())
            a12 <- TypeCell.pair(a1, a2)
            b12 <- TypeCell.pair(b1, b2)
            _ <- unifyTypes(a, a12)
            _ <- unifyTypes(b, b12)
            _ <- connectar(a1, f1, b1, ctx)
            _ <- connectar(a2, f2, b2, ctx)
          } yield ()
        go[a1, a2, b1, b2](par.f1, par.f2)

      case e: EitherF[a1, a2, B] =>
        def go[A1, A2](g: Fun[A1, B], h: Fun[A2, B]): F[Unit] =
          for {
            a1  <- newTypeCell(newTypeVar())
            a2  <- newTypeCell(newTypeVar())
            a12 <- TypeCell.sum(a1, a2)
            _   <- unifyTypes(a, a12)
            _   <- connectar(a1, g, b, ctx)
            _   <- connectar(a2, h, b, ctx)
          } yield ()
        go[a1, a2](e.f, e.g)

      case i: InjectL[A, b2] =>
        for {
          b2  <- newTypeCell(newTypeVar())
          ab2 <- TypeCell.sum(a, b2)
          _   <- unifyTypes(b, ab2)
        } yield ()

      case i: InjectR[A, b1] =>
        for {
          b1  <- newTypeCell(newTypeVar())
          b1a <- TypeCell.sum(b1, a)
          _   <- unifyTypes(b, b1a)
        } yield ()

      case fix: FixF[g] =>
        for {
          g  <- newType1Cell(newType1Var())
          fg <- TypeCell.fix(g)
          _  <- unifyTypes(b, fg)
          // g1 <- abs(ITypeFun.fix(g), a)
          _  <- propagateType(???, g)
        } yield ()

      case unfix: UnfixF[g] =>
        for {
          g  <- newType1Cell(newType1Var())
          fg <- TypeCell.fix(g)
          _  <- unifyTypes(a, fg)
          // g1 <- abs(ITypeFun.fix(g), b)
          _  <- propagateType(???, g)
        } yield ()

      case Rec(label, f) =>
        connectar(a, f, b, ctx.updated(label, (a, b)))

      case RecCall(label) =>
        ctx.get(label) match {
          case Some((ta, tb)) => unifyTypes(a, ta) *> unifyTypes(b, tb)
          case None           => throw new IllegalArgumentException("Recursive call to an undefined function")
        }

      case ConstInt(_) =>
        for {
          _ <- refineUnify(a, IType.Unit)
          _ <- refineUnify(b, IType.Int)
        } yield ()

      case AddInts() =>
        for {
          i  <- newTypeCell[Tag.Int](IType.Int)
          ii <- TypeCell.pair(i, i)
          _ <- unifyTypes(a, ii)
          _ <- refineUnify(b, IType.Int)
        } yield ()

      case IntToString() =>
        for {
          _ <- refineUnify(a, IType.Int)
          _ <- refineUnify(b, IType.String)
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
      case gte.InferenceVar(aliases)     => monadOut.pure(TypeExpr.inferenceVar(aliases))
      case gte.BiApp(op, a1, a2) => (outputTypeExprCell(a1) |@| outputTypeExprCell(a2)) { (a1, a2) => TypeExpr.biApp(op, a1, a2) }
      // case gte.Id()             => monadOut.pure(TypeExpr.id)
      // case gte.Par(f1, f2)      => (outputTypeExprCell(f1) |@| outputTypeExprCell(f2)) { (f1, f2) => TypeExpr.par(f1, f2) }
      // case gte.Dup()            => monadOut.pure(TypeExpr.dup)
      // case gte.Pair()           => monadOut.pure(TypeExpr.pair)
      // case gte.Sum()            => monadOut.pure(TypeExpr.sum)
      // case gte.IntroFst()       => monadOut.pure(TypeExpr.introFst)
      case gte.UnitType()       => monadOut.pure(TypeExpr.unit)
      case gte.IntType()        => monadOut.pure(TypeExpr.int)
      case gte.StringType()     => monadOut.pure(TypeExpr.string)
      case gte.Fix(f, g)        => outputTypeExprCell(g).map(TypeExpr.fix(f, _))
      case gte.PFix(f, g)       => outputTypeExprCell(g).map(TypeExpr.pfix(f, _))
      case gte.TypeError(msg)   => monadOut.point(TypeExpr.typeError(msg))
    }
  }

  private def outputTypeCell(tc: TypeCell): Out[Typ] =
    outputTypeFunCell(tc)

  private def outputTypeFunCell[K, L](tc: TypeFunCell[K, L]): Out[TypeFun[K, L]] =
    iOut(tc).flatMap(t => outputType(t.value))

  private def outputType[K, L, I](
    t: ITypeFun[K, L, I],
  ): Out[TypeFun[K, L]] = {
    import generic.{TypeFun => gtf}
    import t.value.{inKind, outKind}

    t.value match {
      case gtf.Var(aliases)     => monadOut.pure(TypeFun.variable(aliases))
      case gtf.AndThen(f, g)    => (outputTypeFunCell(f) |@| outputTypeFunCell(g)) { (f, g) => f > g }
      case gtf.Id()             => monadOut.pure(TypeFun.id)
      case gtf.Par(f1, f2)      => (outputTypeFunCell(f1) |@| outputTypeFunCell(f2)) { (f1, f2) => TypeFun.par(f1, f2) }
      case gtf.Dup()            => monadOut.pure(TypeFun.dup)
      // case gtf.Pair()           => monadOut.pure(TypeFun.pair)
      // case gtf.Sum()            => monadOut.pure(TypeFun.sum)
      case gtf.IntroFst()       => monadOut.pure(TypeFun.introFst)
      case gtf.UnitType()       => monadOut.pure(TypeFun.unit)
      case gtf.IntType()        => monadOut.pure(TypeFun.int)
      case gtf.StringType()     => monadOut.pure(TypeFun.string)
      case gtf.Fix(f)           => outputTypeFunCell(f).map(TypeFun.fix(_))
      case gtf.PFix(f)          => outputTypeFunCell(f).map(TypeFun.pfix(_))
      case gtf.TypeError(msg)   => monadOut.point(TypeFun.typeError(msg))
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

  private def unifyTypes(
    t1: TypeCell,
    t2: TypeCell,
  ): F[Unit] =
    propagateType(t1, t2) *> propagateType(t2, t1)

  private def propagateType[K, L](
    src: TypeFunCell[K, L],
    tgt: TypeFunCell[K, L],
  ): F[Unit] =
    if (src == tgt) { // TODO: use Equal typeclass on cells (add it to Propagation)
      ().pure[F]
    } else {
      iObserve(iReadOnly(src)).by_ {
        P.iContinually[ITypeFun[K, L, *], ITypeFun.Change[K, L, *, *]] {
          [i] => (t: ITypeFun[K, L, i]) => refinePropagate[K, L, i](tgt, t)
        }
      }
    }

  private def refineUnify[J](
    cell: TypeCell,
    update: IType[J],
  ): F[Unit] = {
    def handleResult[I, K](
      update: IType[J],
      updRes: ITypeFun.UpdRes[○, ●, I, J, K],
    ): F[Unit] = {
      import generic.TypeFun.UpdRes._

      updRes match {
        case UpdatedAliases(_, _)   => throw new AssertionError("did not expect update to be a variable")
        case AlreadySuperset(_)     => throw new AssertionError("did not expect update to be a variable")
        case AlreadyRefined(_)      => throw new AssertionError("did not expect update to be a variable")
        // case AlreadySamePrimitive() => M.pure(())
        case SubstitutedForVar(_)   => M.pure(())
        // case AlreadyTypeApp(ta)     => unifyTypeApps(ta, update)
        // case AlreadyFixType(ft)     => unifyFixTypes(ft, update)
        // case Failed(_)              => M.pure(())
        // case AlreadyFailed(_)       => M.pure(())
        case other => throw new NotImplementedError(s"$other")
      }
    }

    for {
      updRes <- iUpdate(cell)(update: IType[J])
      _      <- handleResult(update, updRes.value)
    } yield ()
  }

  private def refinePropagate[K, L, J](
    cell: TypeFunCell[K, L],
    update: ITypeFun[K, L, J],
  ): F[Unit] = {
    def handleResult[I0, I1](
      update: ITypeFun[K, L, J],
      updRes: ITypeFun.UpdRes[K, L, I0, J, I1],
    ): F[Unit] = {
      import generic.TypeFun.UpdRes._

      updRes match {
        case UpdatedAliases(_, _)   => M.pure(())
        case AlreadySuperset(_)     => M.pure(())
        case AlreadyRefined(_)      => M.pure(())
        case AlreadySameAtom()      => M.pure(())
        case SubstitutedForVar(_)   => M.pure(())
        // case AlreadyAndThen(f)      => propagateAndThen(update, ITypeFun(f))
        case ap: AlreadyPar[TypeFunCell, k1, k2, l1, l2] =>
          propagatePar[k1, k2, l1, l2](update, ITypeFun(ap.value))
        case AlreadyFix(f)          => propagateFix(update, ITypeFun(f))
        // case Failed(_)              => M.pure(())
        // case AlreadyFailed(_)       => M.pure(())
        case other => throw new NotImplementedError(s"$other")
      }
    }

    for {
      updRes <- iUpdate(cell)(update)
      _      <- handleResult(update, updRes.value)
    } yield ()
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
        case UpdatedAliases(_, _)   => M.pure(())
        case AlreadySuperset(_)     => M.pure(())
        case AlreadyRefined(_)      => M.pure(())
        case AlreadySameAtom()      => M.pure(())
        case SubstitutedForVar(_)   => M.pure(())
        case AlreadyBiApp(_, a1, a2, u1, u2) => propagateTpe(u1, a1) *> propagateTpe(u2, a2)
        // case ap: AlreadyPar[TypeExprCell, k1, k2, l1, l2] =>
        //   propagatePar[k1, k2, l1, l2](update, ITypeFun(ap.value))
        case AlreadyFix(_, t, u)    => propagateTpe(u, t)
        // case Failed(_)              => M.pure(())
        // case AlreadyFailed(_)       => M.pure(())
        case other => throw new NotImplementedError(s"$other")
      }
    }

    for {
      updRes <- iUpdate(cell)(update)
      _      <- handleResult(update, updRes.value)
    } yield ()
  }

  // private def propagateAndThen[K, L](
  //   src: ITypeFun[K, L, Tag.Comp],
  //   tgt: ITypeFun[K, L, Tag.Comp],
  // ): F[Unit] = {
  //   import generic.{TypeFun => gtf}

  //   def go[P, Q](src: gtf.AndThen[TypeFunCell, K, P, L], tgt: gtf.AndThen[TypeFunCell, K, Q, L]): F[Unit] = {
  //     val p: Kind[P] = src.pivotKind
  //     val q: Kind[Q] = tgt.pivotKind

  //     (p testEqual q) match {
  //       case Some(p_eq_q) =>
  //         val srcf: TypeFunCell[K, Q] = p_eq_q.substituteCo(src.f)
  //         val srcg: TypeFunCell[Q, L] = p_eq_q.substituteCo[TypeFunCell[*, L]](src.g)
  //         propagateType(srcf, tgt.f) *> propagateType(srcg, tgt.g)
  //       case None =>
  //         import tgt.{inKind, outKind, pivotKind}
  //         refinePropagate(tgt.f, ITypeFun.typeError(s"Output kind mismatch: $p vs. $q")) *>
  //         refinePropagate(tgt.g, ITypeFun.typeError(s"Input kind mismatch: $p vs. $q"))
  //     }
  //   }

  //   (src.value, tgt.value) match {
  //     case (src @ gtf.AndThen(_, _), tgt @ gtf.AndThen(_, _)) => go(src, tgt)
  //   }
  // }

  private def propagatePar[K1, K2, L1, L2](
    src: ITypeFun[K1 × K2, L1 × L2, Tag.Par],
    tgt: ITypeFun[K1 × K2, L1 × L2, Tag.Par],
  ): F[Unit] = {
    import generic.{TypeFun => gtf}

    def go(src: gtf.Par[TypeFunCell, K1, K2, L1, L2], tgt: gtf.Par[TypeFunCell, K1, K2, L1, L2]): F[Unit] = {
      propagateType(src.f, tgt.f) *> propagateType(src.g, tgt.g)
    }

    (src.value, tgt.value) match {
      case (src @ gtf.Par(_, _), tgt @ gtf.Par(_, _)) => go(src, tgt)
    }
  }

  private def propagateFix(
    src: ITypeFun[○, ●, Tag.Fix],
    tgt: ITypeFun[○, ●, Tag.Fix],
  ): F[Unit] = {
    import generic.{TypeFun => gtf}

    def go(src: gtf.Fix[TypeFunCell], tgt: gtf.Fix[TypeFunCell]): F[Unit] =
      propagateType(src.f, tgt.f)

    (src.value, tgt.value) match {
      case (src @ gtf.Fix(_), tgt @ gtf.Fix(_)) => go(src, tgt)
    }
  }

  // private def propagateFix(
  //   src: ITypeExpr[○, ●, Tag.Fix],
  //   tgt: ITypeExpr[○, ●, Tag.Fix],
  // ): F[Unit] = {
  //   import generic.{TypeExpr => gt}

  //   def go(src: gt.Fix[TypeExprCell], tgt: gt.Fix[TypeExprCell]): F[Unit] =
  //     propagateTpe(src.f, tgt.f)

  //   (src.value, tgt.value) match {
  //     case (src @ gt.Fix(_, _), tgt @ gt.Fix(_, _)) => go(src, tgt)
  //   }
  // }

  private def onComplete[A](pa: Var[PromiseOnce[A]])(f: Either[PromiseOnce.Conflict.type, A] => F[Unit]): F[Unit] =
    P.observe(pa).by_(
      P.threshold {
        case PromiseOnce.Empty => None
        case PromiseOnce.Completed(a) => Some(f(Right(a)))
        case PromiseOnce.Conflict => Some(f(Left(PromiseOnce.Conflict)))
      }
    )

  extension [A](pa: Var[PromiseOnce[A]]) {
    def complete(a: A): F[Unit] =
      P.update(pa).by(PromiseOnce.completed(a))
  }

  /** Execute `f` when `cell` is more refined than a variable. */
  private def whenRefined[K, L](cell: TypeFunCell[K, L])(f: ITypeFun[K, L, ?] => F[Unit]): F[Unit] = {
    import generic.{TypeFun => gtf}
    type D[I] = ITypeFun[K, L, I]
    type Δ[I, J] = ITypeFun.Change[K, L, I, J]

    val observer: [i] => D[i] => ITrigger[D, Δ, i] =
      P.iThreshold[D, Δ] {
        [i] => (t: D[i]) =>
          t.value match {
            case gtf.Var(_) => None
            case _          => Some(f(t))
          }
      }

    iObserve(iReadOnly(cell)).by_(observer)
  }
}
