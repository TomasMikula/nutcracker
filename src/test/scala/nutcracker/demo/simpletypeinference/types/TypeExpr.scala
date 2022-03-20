package nutcracker.demo.simpletypeinference.types

import scalaz.Monad
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.kinds._

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

  def fix[K](f: Routing[●, K], g: TypeExpr[K, ●]): TypeExpr[○, ●] =
    TypeExpr(generic.TypeExpr.Fix(f, g))

  def pfix[K, X](f: Routing[K × ●, X], g: TypeExpr[X, ●]): TypeExpr[K, ●] =
    TypeExpr(generic.TypeExpr.PFix(f, g))

  def scalaTypeParam[T](filename: String, line: Int, name: String): TypeExpr[○, ●] =
    TypeExpr(generic.TypeExpr.ScalaTypeParams.one(filename, line, name))

  def typeError[K: Kind, L: OutputKind](msg: String): TypeExpr[K, L] =
    TypeExpr(generic.TypeExpr.TypeError(msg))
}
