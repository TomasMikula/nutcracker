package nutcracker.util

import nutcracker.util.HList.{::, HNil}
import scalaz.{~>, Const}
import scalaz.Id.Id

trait Transformed[L <: HList, F[_], G[_]] extends Serializable {
  type Out <: HList

  def map(nt: F ~> G, fa: L): Out
}

object Transformed {
  type Aux[L1 <: HList, F[_], G[_], L2] = Transformed[L1, F, G] { type Out = L2 }

  type Mapped[L <: HList, F[_]] = Transformed[L, Id, F]
  object Mapped {
    type Aux[L <: HList, F[_], Out0 <: HList] = Transformed.Aux[L, Id, F, Out0]
  }

  type Comapped[L <: HList, F[_]] = Transformed[L, F, Id]
  object Comapped {
    type Aux[L <: HList, F[_], Out0 <: HList] = Transformed.Aux[L, F, Id, Out0]
  }

  def apply[L <: HList, F[_], G[_]](implicit tr: Transformed[L, F, G]) = tr

  implicit def hnilTransformed1[F[_], G[_]]: Transformed.Aux[HNil, F, G, HNil] =
    new Transformed[HNil, F, G] {
      type Out = HNil
      def map(f: F ~> G, fa: HNil): HNil = HNil
    }

  implicit def hnilTransformed2[F[_], A]: Transformed.Aux[HNil, F, Const[A, *], HNil] =
    new Transformed[HNil, F, Const[A, *]] {
      type Out = HNil
      def map(f: F ~> Const[A, *], fa: HNil): HNil = HNil
    }

  implicit def hconsTransformed1[H, T <: HList, F[_], G[_]](implicit tr : Transformed[T, F, G]) =
    new Transformed[F[H] :: T, F, G] {
      type Out = G[H] :: tr.Out
      def map(f: F ~> G, fa: F[H] :: T): Out = f(fa.head) :: tr.map(f, fa.tail)
    }

  implicit def hconsTransformed2[H, T <: HList, G[_]](implicit tr : Transformed[T, Id, G]) =
    new Transformed[H :: T, Id, G] {
      type Out = G[H] :: tr.Out
      def map(f: Id ~> G, fa: H :: T): Out = f(fa.head) :: tr.map(f, fa.tail)
    }

  implicit def hconsTransformed3[H, T <: HList, F[_]](implicit tr : Transformed[T, F, Id]) =
    new Transformed[F[H] :: T, F, Id] {
      type Out = H :: tr.Out
      def map(f: F ~> Id, fa: F[H] :: T): Out = f(fa.head) :: tr.map(f, fa.tail)
    }

  implicit def hconsTransformed4[H, T <: HList, F[_], A](implicit tr : Transformed[T, F, Const[A, *]]) =
    new Transformed[F[H] :: T, F, Const[A, *]] {
      type Out = A :: tr.Out
      def map(f: F ~> Const[A, *], fa: F[H] :: T): Out = f(fa.head).getConst :: tr.map(f, fa.tail)
    }

  implicit def hconsTransformed5[H, T <: HList, A](implicit tr : Transformed[T, Id, Const[A, *]]) =
    new Transformed[H :: T, Id, Const[A, *]] {
      type Out = A :: tr.Out
      def map(f: Id ~> Const[A, *], fa: H :: T): Out = f(fa.head).getConst :: tr.map(f, fa.tail)
    }
}