package nutcracker.rel

import algebra.Order
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.{FreeK, FunctorKA, InjectK, Mapped, StepT, SummonHList}

import scala.language.higherKinds
import shapeless.HList

import scalaz.~>

sealed trait RelLang[K[_], A]

object RelLang {

  case class Relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: OS) extends RelLang[K, Unit] {
    val orders: OS = os
    val ordersWitness: Mapped.Aux[L, Order, OS] = m
  }
  case class OnPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V], h: V => K[Unit]) extends RelLang[K, Unit]

  def relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: SummonHList[OS]): RelLang[K, Unit] = Relate(rel, values)(m, os.get)
  def onPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V])(h: V => K[Unit]): RelLang[K, Unit] = OnPatternMatch(p, a, h)


  private type FR[A] = FreeK[RelLang, A]

  def relateF[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateBuilder[L, m.Out] = RelateBuilder(rel)(m)
  case class RelateBuilder[L <: HList, OS <: HList] private[rel] (rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS]) {
    def values(vals: L)(implicit os: SummonHList[OS]): FreeK[RelLang, Unit] = FreeK.suspend(relate[FR, L, OS](rel, vals))
  }

  def onPatternMatchF[F[_[_], _], V <: HList](p: Pattern[V])(h: V => FreeK[F, Unit])(implicit inj: InjectK[RelLang, F]): FreeK[F, Unit] =
    FreeK.lift(onPatternMatch[FreeK[F, ?], V](p, p.emptyAssignment)(h))
  def onPatternMatchF[F[_[_], _], V <: HList](p: PartiallyAssignedPattern[V])(h: V => FreeK[F, Unit])(implicit inj: InjectK[RelLang, F]): FreeK[F, Unit] =
    FreeK.lift(onPatternMatch[FreeK[F, ?], V](p.pattern, p.assignment)(h))


  implicit def functorKInstance: FunctorKA[RelLang] = new FunctorKA[RelLang] {
    def transform[K[_], L[_], A](rk: RelLang[K, A])(f: K ~> L): RelLang[L, A] = rk match {
      case r @ Relate(rel, values) => Relate(rel, values)(r.ordersWitness, r.orders)
      case OnPatternMatch(p, a, h) => onPatternMatch(p, a)(v => f(h(v)))
    }
  }

  implicit def interpreter: StepT.Step[RelLang, RelDB] = RelDB.interpreter

}