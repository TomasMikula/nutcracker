package nutcracker.rel

import scala.language.higherKinds

import algebra.Order
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.{Mapped, SummonHList}
import shapeless.HList

trait Relations[M[_]] {
  def relate[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateBuilder[L, m.Out]
  def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => M[Unit]): M[Unit]

  final def onPatternMatch[V <: HList](p: Pattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p, p.emptyAssignment)(h)

  final def onPatternMatch[V <: HList](p: PartiallyAssignedPattern[V])(h: V => M[Unit]): M[Unit] =
    onPatternMatch(p.pattern, p.assignment)(h)

  trait RelateBuilder[L <: HList, OS <: HList] {
    def values(vals: L)(implicit os: SummonHList[OS]): M[Unit]
  }
}

object Relations {
  def apply[M[_]](implicit ev: Relations[M]): Relations[M] = ev
}