package nutcracker.rel

import algebra.Order
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.Mapped
import nutcracker.util.free.{InjectK, FreeK}

import scala.language.higherKinds

import shapeless.HList

sealed trait RelLang[K[_], A]

object RelLang {

  case class Relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: OS) extends RelLang[K, Unit] {
    val orders: OS = os
    val ordersWitness: Mapped.Aux[L, Order, OS] = m
  }
  case class OnPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V], h: V => K[Unit]) extends RelLang[K, Unit]

  def relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: OS): RelLang[K, Unit] = Relate(rel, values)
  def onPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V], h: V => K[Unit]): RelLang[K, Unit] = OnPatternMatch(p, a, h)


  private type FR[A] = FreeK[RelLang, A]

  def relateF[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateBuilder[L, m.Out] = RelateBuilder(rel)(m)
  case class RelateBuilder[L <: HList, OS <: HList] private[rel] (rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS]) {
    def apply(values: L)(implicit os: OS): FreeK[RelLang, Unit] = FreeK.suspend(relate[FR, L, OS](rel, values))
  }

  def onPatternMatchF[F[_[_], _], V <: HList](p: Pattern[V])(h: V => FreeK[F, Unit])(implicit inj: InjectK[RelLang, F]): FreeK[F, Unit] =
    FreeK.lift(onPatternMatch[FreeK[F, ?], V](p, p.emptyAssignment, h))

}