package nutcracker.util

import shapeless._
import shapeless.ops.hlist.At

trait Ptr[L <: HList, N <: Nat] extends At[L, N] {
  def index: Int
}

object Ptr {
  def apply[L <: HList, N <: Nat](implicit at: Ptr[L, N]): Aux[L, N, at.Out] = at

  type Aux[L <: HList, N <: Nat, Out0] = Ptr[L, N] { type Out = Out0 }

  implicit def hlistAtZero[H, T <: HList]: Aux[H :: T, _0, H] =
    new Ptr[H :: T, _0] {
      type Out = H
      def apply(l : H :: T): Out = l.head
      def index = 0
    }

  implicit def hlistAtN[H, T <: HList, N <: Nat](implicit
      att: Ptr[T, N]): Aux[H :: T, Succ[N], att.Out] =
    new Ptr[H :: T, Succ[N]] {
      type Out = att.Out
      def apply(l : H :: T) : Out = att(l.tail)
      def index = att.index + 1
    }
}