package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

sealed trait TypeFun[K, L] {
  type X
  def pre: Routing[K, X]
  def expr: TypeExpr[X, L]

  def ∘[J](that: TypeFun[J, K]): TypeFun[J, L] =
    this.pre match {
      case Routing.Id() => TypeFun(that.pre, this.expr ∘ that.expr)
    }
}

object TypeFun {
  def apply[K, P, L](r: Routing[K, P], f: TypeExpr[P, L]): TypeFun[K, L] =
    new TypeFun[K, L] {
      override type X = P
      override def pre = r
      override def expr = f
    }

  def unapply[K, L](f: TypeFun[K, L]): (Routing[K, f.X], TypeExpr[f.X, L]) =
    (f.pre, f.expr)

  def fromExpr[K, L](e: TypeExpr[K, L]): TypeFun[K, L] = {
    import e.inKind
    TypeFun(Routing.id[K], e)
  }

  def toExpr[L](f: TypeFun[○, L]): TypeExpr[○, L] =
    Routing.proveId(f.pre).substituteCo[TypeExpr[*, L]](f.expr)

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
