package nutcracker.rel

import algebra.Order
import nutcracker.util.{FreeK, FunctorKA, InjectK, Mapped, Step, SummonHList}

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


  implicit def functorKInstance: FunctorKA[RelLang] = new FunctorKA[RelLang] {
    def transform[K[_], L[_], A](rk: RelLang[K, A])(f: K ~> L): RelLang[L, A] = rk match {
      case r @ Relate(rel, values) => Relate(rel, values)(r.ordersWitness, r.orders)
      case OnPatternMatch(p, a, h) => onPatternMatch(p, a)(v => f(h(v)))
    }
  }

  implicit def interpreter: Step[RelLang, RelDB] = RelDB.interpreter

  implicit def relationsInstance[F[_[_], _]](implicit inj: InjectK[RelLang, F]): Relations[FreeK[F, ?]] =
    new Relations[FreeK[F, ?]] {
      def relate[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): RelateBuilder[L, m.Out] =
        new RelateBuilder[L, m.Out] {
          def values(vals: L)(implicit os: SummonHList[m.Out]): FreeK[F, Unit] =
            FreeK.injLiftF(RelLang.relate[FreeK[F, ?], L, m.Out](rel, vals)(m, os))
        }

      def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.injLiftF(RelLang.onPatternMatch[FreeK[F, ?], V](p, a)(h))
    }
}