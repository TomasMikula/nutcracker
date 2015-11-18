package nutcracker.util

import scala.language.existentials
import scala.language.higherKinds

import shapeless._
import shapeless.poly.{~>, ~>>}

trait Mapped[L <: HList, F[_]] extends Serializable {
  type Out <: HList

  def remap[G[_]](l: Out, f: F ~> G): (OutG, Mapped.Aux[L, G, OutG]) forSome { type OutG <: HList }
  def toList[A](l: Out, f: F ~>> A): List[A]
}

object Mapped {
  def apply[L <: HList, F[_]](implicit mapped: Mapped[L, F]): Aux[L, F, mapped.Out] = mapped

  type Aux[L <: HList, F[_], Out0 <: HList] = Mapped[L, F] { type Out = Out0 }

  implicit def hnilMapped[F[_]]: Aux[HNil, F, HNil] = new Mapped[HNil, F] {
    type Out = HNil
    def remap[G[_]](l: HNil, f: F ~> G): (HNil, Mapped.Aux[HNil, G, HNil]) = (HNil, implicitly[Mapped.Aux[HNil, G, HNil]])
    def toList[A](l: HNil, f: F ~>> A): List[A] = Nil
  }

  implicit def hconsMapped[H, T <: HList, F[_], OutM <: HList](implicit mt: Mapped.Aux[T, F, OutM]): Aux[H :: T, F, F[H] :: OutM] =
    new Mapped[H :: T, F] {
      type Out = F[H] :: OutM
      def remap[G[_]](l: F[H] :: OutM, f: F ~> G): (OutG, Mapped.Aux[H :: T, G, OutG]) forSome { type OutG <: HList } = {
        val tRes: (Z, Mapped.Aux[T, G, Z]) forSome { type Z <: HList } = mt.remap(l.tail, f)
        def aux[Z <: HList](res: (Z, Mapped.Aux[T, G, Z])) = (f(l.head) :: res._1, hconsMapped[H, T, G, Z](res._2))
        aux(tRes)
      }
      def toList[A](l: F[H] :: OutM, f: F ~>> A): List[A] = f(l.head) :: mt.toList(l.tail, f)
    }
}