package nutcracker.toolkit

import nutcracker.{Assignment, Pattern, Recipe, Rel, Relations}
import nutcracker.util.{ContU, FreeK, HList, Inject, Mapped, MappedListBuilder, SummonHList}
import scalaz.Order
import scalaz.std.option._

private[toolkit] sealed trait RelLang[K[_], A]

private[toolkit] object RelLang {

  case class Relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: OS) extends RelLang[K, Unit] {
    val orders: OS = os
    val ordersWitness: Mapped.Aux[L, Order, OS] = m
  }
  case class OnPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V], h: V => K[Unit]) extends RelLang[K, Unit]
  case class ExecWith[K[_], L <: HList, L0 >: L <: HList, C <: HList, OS <: HList](rel: Rel.Aux[L, L0], ass: Assignment[L], supply: RelToken[L] => K[Unit], exec: L => K[Unit], m: Mapped.Aux[L0, Order, OS], os: OS) extends RelLang[K, Unit]
  case class Supply[K[_], L <: HList](rel: Rel[L], token: RelToken[L], value: L) extends RelLang[K, Unit]

  def relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: SummonHList[OS]): RelLang[K, Unit] = Relate(rel, values)(m, os.get)
  def onPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V])(h: V => K[Unit]): RelLang[K, Unit] = OnPatternMatch(p, a, h)
  def execWith[K[_], L <: HList, C <: HList, OS <: HList](rel: Rel[L], ass: Assignment[L], supply: RelToken[L] => K[Unit], exec: L => K[Unit])(implicit m: Mapped.Aux[rel.Projection, Order, OS], os: OS): RelLang[K, Unit] = ExecWith[K, L, rel.Projection, C, OS](rel, ass, supply, exec, m, os)
  def supply[K[_], L <: HList](rel: Rel[L], token: RelToken[L], value: L): RelLang[K, Unit] = Supply(rel, token, value)


  implicit def relationsInstance[F[_[_], _]](implicit inj: Inject[RelLang[FreeK[F, *], *], F[FreeK[F, *], *]]): Relations[FreeK[F, *]] =
    new Relations[FreeK[F, *]] {
      def relateImpl[L <: HList, OrderL <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): FreeK[F, Unit] =
        FreeK.liftF(inj(RelLang.relate[FreeK[F, *], L, OrderL](rel, values)(m, os)))

      def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.liftF(inj(RelLang.onPatternMatch[FreeK[F, *], V](p, a)(h)))


      def establishImpl[L <: HList, C <: HList, OrderL <: HList](rel: Rel[L], values: C, recipe: Recipe[L, C, FreeK[F, *]])(implicit L: MappedListBuilder[L], m: Mapped.Aux[rel.Projection, Order, OrderL], os: SummonHList[OrderL]): ContU[FreeK[F, *], L] = {
        val noneL_ev = Mapped.empty[L, Option]
        val someC_ev = Mapped.pure[C, Option](values)
        val ass: Assignment[L] = Assignment[L].from(recipe.choose.lift[Option](noneL_ev._2, someC_ev._2).set(noneL_ev._1, someC_ev._1))(noneL_ev._2)
        val supply: RelToken[L] => FreeK[F, Unit] = tok => recipe.create(values).flatMap(l => this.supply(rel, tok, l))
        ContU(f => FreeK.liftF(inj(RelLang.execWith(rel, ass, supply, f)(m, os.get))))
      }

      private def supply[L <: HList](rel: Rel[L], t: RelToken[L], l: L): FreeK[F, Unit] =
        FreeK.liftF(inj(RelLang.supply[FreeK[F, *], L](rel, t, l)))
    }
}