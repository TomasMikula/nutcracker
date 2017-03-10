package nutcracker.rel

import scala.language.higherKinds
import algebra.Order
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.{Mapped, SummonHList}
import shapeless.HList

trait Relations[M[_]] {
  def relateImpl[L <: HList, OrderL <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): M[Unit]
  def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => M[Unit]): M[Unit]

  def relate[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateSyntaxHelper[L, m.Out] =
    new RelateSyntaxHelper[L, m.Out](rel)(m)

  final def onPatternMatch[V <: HList](p: Pattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p, p.emptyAssignment)(h)

  final def onPatternMatch[V <: HList](p: PartiallyAssignedPattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p.pattern, p.assignment)(h)


  final class RelateSyntaxHelper[L <: HList, OS <: HList](rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS]) {
    def values(vals: L)(implicit os: SummonHList[OS]): M[Unit] = relateImpl[L, OS](rel, vals)
  }
}

object Relations {
  def apply[M[_]](implicit ev: Relations[M]): Relations[M] = ev
}