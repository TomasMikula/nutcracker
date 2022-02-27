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

  def reconstructTypes[A, B](f: Fun[A, B]): (Typ, Typ) =
    PropagationToolkit.run { [F[_]] =>
      (propagation: Propagation[F]) =>
        SimpleTypeInference(using propagation).reconstructTypes(f)
    }
}

class SimpleTypeInference[F[_], Propagation <: nutcracker.Propagation[F]](using val P: Propagation) {
  import P.{ITrigger, IVar, M, Out, Var, iFire, iObserve, iOut, iReadOnly, iSleep, iUpdate, monadOut, newCell, readOnly}

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
      ITypeFun(generic.TypeFun.Pair())

    def sum: ITypeFun[● × ●, ●, Tag.Sum] =
      ITypeFun(generic.TypeFun.Sum())

    def par[K1: Kind, K2: Kind, L1: Kind, L2: Kind](
      f1: TypeFunCell[K1, L1],
      f2: TypeFunCell[K2, L2],
    ): ITypeFun[K1 × K2, L1 × L2, Tag.Par] =
      ITypeFun(generic.TypeFun.Par(f1, f2))

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

  // type TypeCell  = IVar[TypeT[IVar, *]]
  // type Type1Cell = IVar[Type1T[IVar, *]]

  // def newTypeVar(): TypeT[IVar, Tag.Var] =
  //   types.newTypeVar[IVar]()

  // def newType1Var(): Type1T[IVar, Tag.Var] =
  //   types.newType1Var[IVar]()

  // def newTypeCell[I](t: TypeT[IVar, I]): F[TypeCell] =
  //   P.newICell(t)

  // def newType1Cell[I](t: Type1T[IVar, I]): F[Type1Cell] =
  //   P.newICell(t)

  def reconstructTypes[A, B](
    f: Fun[A, B]
  ): F[Out[(Typ, Typ)]] =
    for {
      ta <- newTypeCell(newTypeVar())
      tb <- newTypeCell(newTypeVar())
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
          newTypeCell(newTypeVar())
            .flatMap { x => connect(a, g, x, ctx) *> connect(x, h, b, ctx) }
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
            _ <- connect(a1, f1, b1, ctx)
            _ <- connect(a2, f2, b2, ctx)
          } yield ()
        go[a1, a2, b1, b2](par.f1, par.f2)

      case e: EitherF[a1, a2, B] =>
        def go[A1, A2](g: Fun[A1, B], h: Fun[A2, B]): F[Unit] =
          for {
            a1  <- newTypeCell(newTypeVar())
            a2  <- newTypeCell(newTypeVar())
            a12 <- TypeCell.sum(a1, a2)
            _   <- unifyTypes(a, a12)
            _   <- connect(a1, g, b, ctx)
            _   <- connect(a2, h, b, ctx)
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
          g1 <- abs(ITypeFun.fix(g), a)
          _  <- propagateType(g1, g)
        } yield ()

      case unfix: UnfixF[g] =>
        for {
          g  <- newType1Cell(newType1Var())
          fg <- TypeCell.fix(g)
          _  <- unifyTypes(a, fg)
          g1 <- abs(ITypeFun.fix(g), b)
          _  <- propagateType(g1, g)
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

  private def outputTypeCell(tc: TypeCell): Out[Typ] =
    outputTypeFunCell(tc)

  // private def outputType1Cell(tc: Type1Cell): Out[TypeFun[●, ●]] =
  //   iOut(tc).flatMap(f => outputType1(f.value))

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
      case gtf.Pair()           => monadOut.pure(TypeFun.pair)
      case gtf.Sum()            => monadOut.pure(TypeFun.sum)
      case gtf.IntroFst()       => monadOut.pure(TypeFun.introFst)
      case gtf.UnitType()       => monadOut.pure(TypeFun.unit)
      case gtf.IntType()        => monadOut.pure(TypeFun.int)
      case gtf.StringType()     => monadOut.pure(TypeFun.string)
      case gtf.Fix(f)           => outputTypeFunCell(f).map(TypeFun.fix(_))
      case gtf.PFix(f)          => outputTypeFunCell(f).map(TypeFun.pfix(_))
      case gtf.TypeError(msg)   => monadOut.point(TypeFun.typeError(msg))
    }
  }

  // private def outputType1[I](
  //   tf: IType1[I],
  // ): Out[TypeFun[●, ●]] = {
  //   ???
  //   // tf.value match {
  //   //   case TypeApp1T(f, x) =>
  //   //     outputTypeCell(x).map(TypeApp1(f, _))
  //   //   case Composed1T(f, g) =>
  //   //     (outputType1Cell(f) |@| outputType1Cell(g)) { (f, g) => Composed1(f, g) }
  //   //   case Type1ErrorT(msg) =>
  //   //     monadOut.pure(Type1Error(msg))
  //   //   case other =>
  //   //     throw new NotImplementedError(s"$other")
  //   // }
  // }

  private def unifyTypes(
    t1: TypeCell,
    t2: TypeCell,
  ): F[Unit] =
    propagateType(t1, t2) *> propagateType(t2, t1)

  // private def unifyTypeConstructors(
  //   g: Type1Cell,
  //   h: Type1Cell,
  // ): F[Unit] =
  //   propagateType1(g, h) *> propagateType1(h, g)

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

  // private def propagateType1(
  //   src: Type1Cell,
  //   tgt: Type1Cell,
  // ): F[Unit] =
  //   if (src == tgt) { // TODO: use Equal typeclass on cells (add it to Propagation)
  //     ().pure[F]
  //   } else {
  //     iObserve(iReadOnly(src)).by_ {
  //       P.iContinually[IType1, ITypeFun.Change[●, ●, *, *]] {
  //         [i] => (t: IType1[i]) => refinePropagate1[i](tgt, t)
  //       }
  //     }
  //   }

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
        case AlreadyAndThen(f)      => propagateAndThen(update, ITypeFun(f))
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

  // private def refinePropagate1[J](
  //   cell: Type1Cell,
  //   update: IType1[J],
  // ): F[Unit] = {
  //   def handleResult[I, J, K](
  //     update: IType1[J],
  //     updRes: ITypeFun.UpdRes[●, ●, I, J, K],
  //   ): F[Unit] = {
  //     import generic.TypeFun.UpdRes._
  //     ???
  //     // updRes match {
  //     //   case Unchanged()          => M.pure(())
  //     //   case SubstitutedForVar(_) => M.pure(())
  //     //   case UnifiedVars(_, _)    => M.pure(())
  //     //   case AlreadyTypeApp(ta)   => propagateTypeArg(ta, update)
  //     //   case other                => throw new NotImplementedError(s"$other")
  //     // }
  //   }

  //   for {
  //     updRes <- iUpdate(cell)(update)
  //     _      <- handleResult(update, updRes.value)
  //   } yield ()
  // }

  private def propagateAndThen[K, L](
    src: ITypeFun[K, L, Tag.Comp],
    tgt: ITypeFun[K, L, Tag.Comp],
  ): F[Unit] = {
    import generic.{TypeFun => gtf}

    def go[P, Q](src: gtf.AndThen[TypeFunCell, K, P, L], tgt: gtf.AndThen[TypeFunCell, K, Q, L]): F[Unit] = {
      val p: Kind[P] = src.pivotKind
      val q: Kind[Q] = tgt.pivotKind

      (p testEqual q) match {
        case Some(p_eq_q) =>
          val srcf: TypeFunCell[K, Q] = p_eq_q.substituteCo(src.f)
          val srcg: TypeFunCell[Q, L] = p_eq_q.substituteCo[TypeFunCell[*, L]](src.g)
          propagateType(srcf, tgt.f) *> propagateType(srcg, tgt.g)
        case None =>
          import tgt.{inKind, outKind, pivotKind}
          refinePropagate(tgt.f, ITypeFun.typeError(s"Output kind mismatch: $p vs. $q")) *>
          refinePropagate(tgt.g, ITypeFun.typeError(s"Input kind mismatch: $p vs. $q"))
      }
    }

    (src.value, tgt.value) match {
      case (src @ gtf.AndThen(_, _), tgt @ gtf.AndThen(_, _)) => go(src, tgt)
    }
  }

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

  // private def unifyTypeApps(
  //   gx: TypeT[IVar, Tag.App],
  //   hy: TypeT[IVar, Tag.App],
  // ): F[Unit] =
  //   propagateTypeApp(gx, hy) *> propagateTypeApp(hy, gx)

  // private def propagateTypeArg(
  //   gx: Type1T[IVar, Tag.App],
  //   hy: Type1T[IVar, Tag.App],
  // ): F[Unit] = {
  //   def go(gx: TypeApp1T[IVar], hy: TypeApp1T[IVar]): F[Unit] =
  //     propagateType(gx.x, hy.x)

  //   (gx, hy) match {
  //     case (gx @ TypeApp1T(_, _), hy @ TypeApp1T(_, _)) => go(gx, hy)
  //   }
  // }

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

  // private def unifyFixTypes(
  //   fg: TypeT[IVar, Tag.Fix],
  //   fh: TypeT[IVar, Tag.Fix],
  // ): F[Unit] =
  //   propagateFixType(fg, fh) *> propagateFixType(fh, fg)

  private def abs(x: IType[Tag.Fix], expr: TypeCell): F[Type1Cell] =
    x.value match {
      case x @ generic.TypeFun.Fix(_) => abs(x, expr)
    }

  private def abs(x: generic.TypeFun.Fix[TypeFunCell], expr: TypeCell): F[Type1Cell] =
    for {
      res <- newType1Cell(newType1Var())
      p   <- find(x, expr)
      _   <- onComplete(p) {
        case Right(FindRes.Found(f)) => refinePropagate(res, f)
        case other => throw new NotImplementedError(s"other")
      }
    } yield res

  private def onComplete[A](pa: Var[PromiseOnce[A]])(f: Either[PromiseOnce.Conflict.type, A] => F[Unit]): F[Unit] =
    P.observe(pa).by_(
      P.threshold {
        case PromiseOnce.Empty => None
        case PromiseOnce.Completed(a) => Some(f(Right(a)))
        case PromiseOnce.Conflict => Some(f(Left(PromiseOnce.Conflict)))
      }
    )

  sealed trait IntroRightMost[K, L]
  object IntroRightMost {}

  /** Result of looking for a given (fixed-point) type (`●`) in a `TypeFunCell[○, L]` */
  sealed trait FindRes[L]
  object FindRes {
    case class Found[L](f: ITypeFun[●, L, ?]) extends FindRes[L]

    case class NotFound[L]() extends FindRes[L]

    /** The unit kind (`○`) is propagated in the right-most position of `L`. */
    case class Propagated[L0, L](f: ITypeFun[○, L0, ?], g: IntroRightMost[L0, L]) extends FindRes[L]
  }

  extension [A](pa: Var[PromiseOnce[A]]) {
    def complete(a: A): F[Unit] =
      P.update(pa).by(PromiseOnce.completed(a))
  }

  private def find[L](x: generic.TypeFun.Fix[TypeFunCell], in: TypeFunCell[○, L]): F[Var[PromiseOnce[FindRes[L]]]] = {
    import generic.{TypeFun => gtf}
    for {
      res <- newCell(PromiseOnce.empty[FindRes[L]])
      _   <- whenRefined(in) { t =>
        t.value match {
          case gtf.Fix(f) =>
            if (f == x.f) { // TODO: use Equal typeclass on cells (should be added to Propagation)
              val L_eq_● = summon[L =:= L].asInstanceOf[L =:= ●]
              res.complete(FindRes.Found(L_eq_●.substituteContra[ITypeFun[●, *, ?]](ITypeFun.id[●])))
            } else {
              res.complete(FindRes.NotFound())
            }
          case other =>
            throw new NotImplementedError(s"$other")
        }
      }
    } yield res
  }

  // private def abs(x: generic.TypeFun.Fix[TypeFunCell], expr: TypeCell): F[Type1Cell] = {
  //   type D[I] = IType[I]
  //   type Δ[I, J] = ITypeFun.Change[○, ●, I, J]

  //   def observer(acc: Option[Type1Cell], resultVar: Type1Cell): [i] => D[i] => ITrigger[D, Δ, i] =
  //     [i] => (t: D[i]) =>
  //       ???
  //       // t.value match {
  //       //   case TypeVar(_) =>
  //       //     iSleep[D, Δ, i]([j] => (t: D[j], _: Δ[i, j]) => observer(acc, resultVar)[j](t))
  //       //   case y @ FixTypeT(f) =>
  //       //     iFire[D, Δ, i] {
  //       //       if (f == x.f) { // TODO: use Equal typeclass on cells (should be added to Propagation)
  //       //         acc match {
  //       //           case Some(g) => propagateType1(g, resultVar)
  //       //           case None    => refinePropagate1(resultVar, IType1.typeError(s"Fixed-point of identity (`[x] =>> x`) is not supported"))
  //       //         }
  //       //       } else {
  //       //         refinePropagate1(resultVar, IType1.typeError(s"Nested fixed-point types are not yet supported"))
  //       //       }
  //       //     }
  //       //   case TypeAppT(f, a) =>
  //       //     iFire[D, Δ, i] {
  //       //       for {
  //       //         acc1 <- acc.map(g => newType1Cell(g ∘ f)).getOrElse(f.pure[F])
  //       //         _    <- go(Some(acc1), a, resultVar)
  //       //       } yield ()
  //       //     }
  //       // }

  //   def go(acc: Option[Type1Cell], expr: TypeCell, resultVar: Type1Cell): F[Unit] =
  //     iObserve(iReadOnly(expr)).by_(observer(acc, resultVar))

  //   for {
  //     res <- newType1Cell(newType1Var())
  //     _   <- go(acc = None, expr, res)
  //   } yield res
  // }

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
