package nutcracker.demo.simpletypeinference

import nutcracker.Propagation
import nutcracker.toolkit.PropagationToolkit
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.ast.Fun
import nutcracker.demo.simpletypeinference.ast.Fun._
import nutcracker.demo.simpletypeinference.types._

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
  import P.{ITrigger, IVar, M, Out, iFire, iObserve, iOut, iReadOnly, iSleep, iUpdate, monadOut, newICell}

  type TypeCell  = IVar[TypeT[IVar, *]]
  type Type1Cell = IVar[Type1T[IVar, *]]

  def reconstructTypes[A, B](
    f: Fun[A, B]
  ): F[Out[(Type, Type)]] =
    for {
      ta <- newICell(newTypeVar[IVar]())
      tb <- newICell(newTypeVar[IVar]())
      _ <- connect(ta, f, tb, Map.empty)
    } yield for {
      ta <- outputTypeCell(ta)
      tb <- outputTypeCell(tb)
    } yield (ta, tb)

  private def connect[A, B](
    a: TypeCell,
    f: Fun[A, B],
    b: TypeCell,
    ctx: Map[Fun.Label[?, ?], (TypeCell, TypeCell)], // TODO: use just a simple Map
  ): F[Unit] =
    f match {
      case IdFun() =>
        unifyTypes(a, b)

      case AndThen(g, h) =>
        def go[X](g: Fun[A, X], h: Fun[X, B]): F[Unit] =
          newICell(newTypeVar[IVar]())
            .flatMap { x => connect(a, g, x, ctx) *> connect(x, h, b, ctx) }
        go(g, h)

      case par: Par[a1, a2, b1, b2] =>
        def go[A1, A2, B1, B2](f1: Fun[A1, B1], f2: Fun[A2, B2]): F[Unit] =
          for {
            a1 <- newICell(newTypeVar[IVar]())
            a2 <- newICell(newTypeVar[IVar]())
            b1 <- newICell(newTypeVar[IVar]())
            b2 <- newICell(newTypeVar[IVar]())
            a1_ <- newICell(ProductTypeT(a1))
            b1_ <- newICell(ProductTypeT(b1))
            _ <- refineUnify(a, a1_(a2))
            _ <- refineUnify(b, b1_(b2))
            _ <- connect(a1, f1, b1, ctx)
            _ <- connect(a2, f2, b2, ctx)
          } yield ()
        go[a1, a2, b1, b2](par.f1, par.f2)

      case e: EitherF[a1, a2, B] =>
        def go[A1, A2](g: Fun[A1, B], h: Fun[A2, B]): F[Unit] =
          for {
            a1  <- newICell(newTypeVar[IVar]())
            a2  <- newICell(newTypeVar[IVar]())
            a1_ <- newICell(SumTypeT(a1))
            _   <- refineUnify(a, a1_(a2))
            _   <- connect(a1, g, b, ctx)
            _   <- connect(a2, h, b, ctx)
          } yield ()
        go[a1, a2](e.f, e.g)

      case i: InjectL[A, b2] =>
        for {
          b2 <- newICell(newTypeVar[IVar]())
          a_ <- newICell(SumTypeT(a))
          _  <- refineUnify(b, a_(b2))
        } yield ()

      case i: InjectR[A, b1] =>
        for {
          b1  <- newICell(newTypeVar[IVar]())
          b1_ <- newICell(SumTypeT(b1))
          _   <- refineUnify(b, b1_(a))
        } yield ()

      case fix: FixF[g] =>
        for {
          g <- newICell(newType1Var[IVar]())
          _  <- refineUnify(b, FixTypeT(g))
          g1 <- abs(FixTypeT(g), a)
          _  <- propagateType1(g1, g)
        } yield ()

      case unfix: UnfixF[g] =>
        for {
          g <- newICell(newType1Var[IVar]())
          _  <- refineUnify(a, FixTypeT(g))
          g1 <- abs(FixTypeT(g), b)
          _  <- propagateType1(g1, g)
        } yield ()

      case Rec(label, f) =>
        connect(a, f, b, ctx.updated(label, (a, b)))

      case RecCall(label) =>
        ctx.get(label) match {
          case Some((ta, tb)) => unifyTypes(a, ta) *> unifyTypes(b, tb)
          case None           => throw new IllegalArgumentException("Recursive call to an undefined function")
        }

      case IntToString() =>
        for {
          _ <- refineUnify(a, IntTypeT())
          _ <- refineUnify(b, StringTypeT())
        } yield ()
    }

  private def outputTypeCell(tc: TypeCell): Out[Type] =
    iOut(tc).flatMap(t => outputType(t.value))

  private def outputType1Cell(tc: Type1Cell): Out[Type1] =
    iOut(tc).flatMap(f => outputType1(f.value))

  private def outputType[I](
    ta: TypeT[IVar, I],
  ): Out[Type] =
    ta match {
      case TypeVar(aliases) => monadOut.point(TypeVar(aliases))
      case UnitTypeT()          => monadOut.point(UnitType())
      case IntTypeT()           => monadOut.point(IntType())
      case StringTypeT()        => monadOut.point(StringType())
      case FixTypeT(f)          => outputType1Cell(f).map(FixType(_))
      case TypeAppT(f, x)       => (outputType1Cell(f) |@| outputTypeCell(x)) { (f, x) => TypeApp(f, x) }
      case TypeErrorT(msg)  => monadOut.point(TypeError(msg))
    }

  private def outputType1[I](
    tf: Type1T[IVar, I],
  ): Out[Type1] =
    tf match {
      case TypeApp1T(f, x) =>
        outputTypeCell(x).map(TypeApp1(f, _))
      case Composed1T(f, g) =>
        (outputType1Cell(f) |@| outputType1Cell(g)) { (f, g) => ComposedConstructors(f, g) }
      case other =>
        throw new NotImplementedError(s"$other")
    }

  private def unifyTypes(
    t1: TypeCell,
    t2: TypeCell,
  ): F[Unit] =
    propagateType(t1, t2) *> propagateType(t2, t1)

  private def unifyTypeConstructors(
    g: Type1Cell,
    h: Type1Cell,
  ): F[Unit] =
    propagateType1(g, h) *> propagateType1(h, g)

  private def propagateType(
    src: TypeCell,
    tgt: TypeCell,
  ): F[Unit] =
    if (src == tgt) { // TODO: use Equal typeclass on cells (add it to Propagation)
      ().pure[F]
    } else {
      iObserve(iReadOnly(src)).by_ {
        P.iContinually[TypeT[IVar, *], TypeT.ChangeT[IVar, *, *]] {
          [i] => (t: TypeT[IVar, i]) => refinePropagate[i](tgt, t)
        }
      }
    }

  private def propagateType1(
    src: Type1Cell,
    tgt: Type1Cell,
  ): F[Unit] =
    if (src == tgt) { // TODO: use Equal typeclass on cells (add it to Propagation)
      ().pure[F]
    } else {
      iObserve(iReadOnly(src)).by_ {
        P.iContinually[Type1T[IVar, *], Type1T.ChangeT[IVar, *, *]] {
          [i] => (t: Type1T[IVar, i]) => refinePropagate1[i](tgt, t)
        }
      }
    }

  private def refineUnify[J](
    cell: TypeCell,
    update: NonVarTypeT[IVar, J],
  ): F[Unit] = {
    def handleResult[I, K](
      update: NonVarTypeT[IVar, J],
      updRes: TypeT.UpdRes[IVar, I, J, K],
    ): F[Unit] = {
      import TypeT.UpdRes._
      updRes match {
        case UnifiedVars(_, _)      => throw new AssertionError("impossible (would mean that J = Tag.Var, which cannot be true)")
        case AlreadySuperset(_)     => throw new AssertionError("impossible (would mean that J = Tag.Var, which cannot be true)")
        case AlreadyRefined(_)      => throw new AssertionError("impossible (would mean that J = Tag.Var, which cannot be true)")
        case AlreadySamePrimitive() => M.pure(())
        case SubstitutedForVar(_)   => M.pure(())
        case AlreadyTypeApp(ta)     => unifyTypeApps(ta, update)
        case AlreadyFixType(ft)     => unifyFixTypes(ft, update)
        case Failed(_)              => M.pure(())
        case AlreadyFailed(_)       => M.pure(())
      }
    }

    for {
      updRes <- iUpdate(cell)(update: TypeT[IVar, J])
      _      <- handleResult(update, updRes.value)
    } yield ()
  }

  private def refinePropagate[J](
    cell: TypeCell,
    update: TypeT[IVar, J],
  ): F[Unit] = {
    def handleResult[I, K](
      update: TypeT[IVar, J],
      updRes: TypeT.UpdRes[IVar, I, J, K],
    ): F[Unit] = {
      import TypeT.UpdRes._
      updRes match {
        case UnifiedVars(_, _)      => M.pure(())
        case AlreadySuperset(_)     => M.pure(())
        case AlreadyRefined(_)      => M.pure(())
        case AlreadySamePrimitive() => M.pure(())
        case SubstitutedForVar(_)   => M.pure(())
        case AlreadyTypeApp(ta)     => propagateTypeApp(ta, update)
        case AlreadyFixType(ft)     => propagateFixType(ft, update)
        case Failed(_)              => M.pure(())
        case AlreadyFailed(_)       => M.pure(())
      }
    }

    for {
      updRes <- iUpdate(cell)(update: TypeT[IVar, J])
      _      <- handleResult(update, updRes.value)
    } yield ()
  }

  private def refinePropagate1[J](
    cell: Type1Cell,
    update: Type1T[IVar, J],
  ): F[Unit] = {
    def handleResult[I, J, K](
      update: Type1T[IVar, J],
      updRes: Type1T.UpdRes[IVar, I, J, K],
    ): F[Unit] = {
      import Type1T.UpdRes._
      updRes match {
        case Unchanged()          => M.pure(())
        case SubstitutedForVar(_) => M.pure(())
        case UnifiedVars(_, _)    => M.pure(())
        case AlreadyTypeApp(ta)   => propagateTypeArg(ta, update)
        case other                => throw new NotImplementedError(s"$other")
      }
    }

    for {
      updRes <- iUpdate(cell)(update)
      _      <- handleResult(update, updRes.value)
    } yield ()
  }

  private def propagateTypeApp(
    gx: TypeT[IVar, Tag.App],
    hy: TypeT[IVar, Tag.App],
  ): F[Unit] = {
    def go(gx: TypeAppT[IVar], hy: TypeAppT[IVar]): F[Unit] =
      propagateType1(gx.f, hy.f) *> propagateType(gx.x, hy.x)

    (gx, hy) match {
      case (gx @ TypeAppT(_, _), hy @ TypeAppT(_, _)) => go(gx, hy)
    }
  }

  private def unifyTypeApps(
    gx: TypeT[IVar, Tag.App],
    hy: TypeT[IVar, Tag.App],
  ): F[Unit] =
    propagateTypeApp(gx, hy) *> propagateTypeApp(hy, gx)

  private def propagateTypeArg(
    gx: Type1T[IVar, Tag.App],
    hy: Type1T[IVar, Tag.App],
  ): F[Unit] = {
    def go(gx: TypeApp1T[IVar], hy: TypeApp1T[IVar]): F[Unit] =
      propagateType(gx.x, hy.x)

    (gx, hy) match {
      case (gx @ TypeApp1T(_, _), hy @ TypeApp1T(_, _)) => go(gx, hy)
    }
  }

  private def propagateFixType(
    fg: TypeT[IVar, Tag.Fix],
    fh: TypeT[IVar, Tag.Fix],
  ): F[Unit] = {
    def go(fg: FixTypeT[IVar], fh: FixTypeT[IVar]): F[Unit] =
      propagateType1(fg.f, fh.f)

    (fg, fh) match {
      case (fg @ FixTypeT(_), fh @ FixTypeT(_)) => go(fg, fh)
    }
  }

  private def unifyFixTypes(
    fg: TypeT[IVar, Tag.Fix],
    fh: TypeT[IVar, Tag.Fix],
  ): F[Unit] =
    propagateFixType(fg, fh) *> propagateFixType(fh, fg)

  private def abs(x: FixTypeT[IVar], expr: TypeCell): F[Type1Cell] = {
    type D[I] = TypeT[IVar, I]
    type Δ[I, J] = TypeT.ChangeT[IVar, I, J]

    def observer(acc: Option[Type1Cell], resultVar: Type1Cell): [i] => D[i] => ITrigger[D, Δ, i] =
      [i] => (t: D[i]) =>
        t match {
          case TypeVar(_) =>
            iSleep[D, Δ, i]([j] => (t: D[j], _: Δ[i, j]) => observer(acc, resultVar)[j](t))
          case y @ FixTypeT(f) =>
            iFire[D, Δ, i] {
              if (f == x.f) { // TODO: use Equal typeclass on cells (should be added to Propagation)
                acc match {
                  case Some(g) => propagateType1(g, resultVar)
                  case None    => refinePropagate1(resultVar, Type1ErrorT(s"Fixed-point of identity (`[x] =>> x`) is not supported"))
                }
              } else {
                refinePropagate1(resultVar, Type1ErrorT(s"Nested fixed-point types are not yet supported"))
              }
            }
          case TypeAppT(f, a) =>
            iFire[D, Δ, i] {
              for {
                acc1 <- acc.map(g => newICell(g ∘ f)).getOrElse(f.pure[F])
                _    <- go(Some(acc1), a, resultVar)
              } yield ()
            }
        }

    def go(acc: Option[Type1Cell], expr: TypeCell, resultVar: Type1Cell): F[Unit] =
      iObserve(iReadOnly(expr)).by_(observer(acc, resultVar))

    for {
      res <- newICell(newType1Var[IVar]())
      _   <- go(acc = None, expr, res)
    } yield res
  }
}
