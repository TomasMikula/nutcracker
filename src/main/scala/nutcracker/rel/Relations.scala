package nutcracker.rel

import scala.language.higherKinds
import algebra.Order
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.{ContU, Mapped, MappedListBuilder, SummonHList}
import scalaz.Bind
import scalaz.syntax.bind._
import shapeless.HList

trait Relations[M[_]] {
  def relateImpl[L <: HList, OrderL <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): M[Unit]
  def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => M[Unit]): M[Unit]

  def establishImpl[L <: HList, C <: HList, OrderL <: HList](rel: Rel[L], values: C, recipe: Recipe[L, C, M])(implicit L: MappedListBuilder[L], m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): ContU[M, L]

  def establish[L <: HList, C <: HList](rel: Rel[L], values: C, recipe: Recipe[L, C, M])(implicit m: Mapped[L, Order]): EstablishSyntaxHelper[L, C, m.Out] =
    new EstablishSyntaxHelper[L, C, m.Out](rel, values, recipe)(m)

  def establish[L <: HList, C <: HList, OrderL <: HList](con: Constraint[L, M], values: C, recipe: Recipe[L, C, M])(implicit M: Bind[M], m: Mapped[L, Order]): EstablishSyntaxHelper[L, C, m.Out] =
    establish[L, C](con.rel, values, recipe.andThen(con.setup))

  def relate[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateSyntaxHelper[L, m.Out] =
    new RelateSyntaxHelper[L, m.Out](rel)(m)

  def constrain[L <: HList](con: Constraint[L, M])(implicit m: Mapped[L, Order]): ConstrainSyntaxHelper[L, m.Out] =
    new ConstrainSyntaxHelper[L, m.Out](con)(m)

  final def onPatternMatch[V <: HList](p: Pattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p, p.emptyAssignment)(h)

  final def onPatternMatch[V <: HList](p: PartiallyAssignedPattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p.pattern, p.assignment)(h)


  final class RelateSyntaxHelper[L <: HList, OS <: HList](rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS]) {
    def values(vals: L)(implicit os: SummonHList[OS]): M[Unit] = relateImpl[L, OS](rel, vals)
  }

  final class ConstrainSyntaxHelper[L <: HList, OS <: HList](con: Constraint[L, M])(implicit m: Mapped.Aux[L, Order, OS]) {
    def values(vals: L)(implicit os: SummonHList[OS], M: Bind[M]): M[Unit] =
      relateImpl(con.rel, vals) >> con.setup(vals)
  }

  final class EstablishSyntaxHelper[L <: HList, C <: HList, OS <: HList](rel: Rel[L], values: C, recipe: Recipe[L, C, M])(implicit m: Mapped.Aux[L, Order, OS]) {
    def cont(implicit L: MappedListBuilder[L], os: SummonHList[OS]): ContU[M, L] =
      establishImpl[L, C, OS](rel, values, recipe)
  }
}

object Relations {
  def apply[M[_]](implicit ev: Relations[M]): Relations[M] = ev
}