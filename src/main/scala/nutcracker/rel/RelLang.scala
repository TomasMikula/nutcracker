package nutcracker.rel

import algebra.Order
import nutcracker.util.{ContU, FreeK, InjectK, Mapped, MappedListBuilder, SummonHList}
import scala.language.higherKinds
import scalaz.std.option._
import shapeless.HList

private[rel] sealed trait RelLang[K[_], A]

private[rel] object RelLang {

  case class Relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: OS) extends RelLang[K, Unit] {
    val orders: OS = os
    val ordersWitness: Mapped.Aux[L, Order, OS] = m
  }
  case class OnPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V], h: V => K[Unit]) extends RelLang[K, Unit]
  case class ExecWith[K[_], L <: HList, C <: HList, OS <: HList](rel: Rel[L], ass: Assignment[L], supply: Token[L] => K[Unit], exec: L => K[Unit], m: Mapped.Aux[L, Order, OS], os: OS) extends RelLang[K, Unit]
  case class Supply[K[_], L <: HList](rel: Rel[L], token: Token[L], value: L) extends RelLang[K, Unit]

  def relate[K[_], L <: HList, OS <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OS], os: SummonHList[OS]): RelLang[K, Unit] = Relate(rel, values)(m, os.get)
  def onPatternMatch[K[_], V <: HList](p: Pattern[V], a: Assignment[V])(h: V => K[Unit]): RelLang[K, Unit] = OnPatternMatch(p, a, h)
  def execWith[K[_], L <: HList, C <: HList, OS <: HList](rel: Rel[L], ass: Assignment[L], supply: Token[L] => K[Unit], exec: L => K[Unit])(implicit m: Mapped.Aux[L, Order, OS], os: OS): RelLang[K, Unit] = ExecWith(rel, ass, supply, exec, m, os)
  def supply[K[_], L <: HList](rel: Rel[L], token: Token[L], value: L): RelLang[K, Unit] = Supply(rel, token, value)


  implicit def relationsInstance[F[_[_], _]](implicit inj: InjectK[RelLang, F]): Relations[FreeK[F, ?]] =
    new Relations[FreeK[F, ?]] {
      def relateImpl[L <: HList, OrderL <: HList](rel: Rel[L], values: L)(implicit m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): FreeK[F, Unit] =
        FreeK.injLiftF(RelLang.relate[FreeK[F, ?], L, OrderL](rel, values)(m, os))

      def onPatternMatch[V <: HList](p: Pattern[V], a: Assignment[V])(h: V => FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.injLiftF(RelLang.onPatternMatch[FreeK[F, ?], V](p, a)(h))


      def establishImpl[L <: HList, C <: HList, OrderL <: HList](rel: Rel[L], values: C, recipe: Recipe[L, C, FreeK[F, ?]])(implicit L: MappedListBuilder[L], m: Mapped.Aux[L, Order, OrderL], os: SummonHList[OrderL]): ContU[FreeK[F, ?], L] = {
        val noneL_ev = Mapped.empty[L, Option]
        val someC_ev = Mapped.pure[C, Option](values)
        val ass: Assignment[L] = Assignment[L].from(recipe.choose.lift[Option](noneL_ev._2, someC_ev._2).set(noneL_ev._1, someC_ev._1))(noneL_ev._2)
        val supply: Token[L] => FreeK[F, Unit] = tok => recipe.create(values) >>= { this.supply(rel, tok, _) }
        ContU(f => FreeK.injLiftF(RelLang.execWith(rel, ass, supply, f)(m, os.get)))
      }

      private def supply[L <: HList](rel: Rel[L], t: Token[L], l: L): FreeK[F, Unit] =
        FreeK.injLiftF(RelLang.supply[FreeK[F, ?], L](rel, t, l))
    }
}