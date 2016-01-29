package nutcracker.util

import shapeless.Nat._0
import shapeless.ops.hlist.Drop
import shapeless.ops.nat.ToInt
import shapeless.{Succ, Nat, HNil, HList, ::}

trait Pointers[L <: HList] {
  type Out <: HList

  def get: Out
}

object Pointers {
  type Aux[L <: HList, Out0 <: HList] = Pointers[L] { type Out = Out0 }

  implicit def pointers[L <: HList](implicit p0: Pointers0[L, L, _0]): Aux[L, p0.Out] = new Pointers[L] {
    type Out = p0.Out

    override def get: Out = p0.get
  }
}

trait Pointers0[L <: HList, T <: HList, N <: Nat] {
  type Out <: HList

  def get: Out
}

object Pointers0 {
  type Aux[L <: HList, T <: HList, N <: Nat, Out0 <: HList] = Pointers0[L, T, N] { type Out = Out0 }

  implicit def hnilPointers[L <: HList, N <: Nat](implicit d: Drop.Aux[L, N, HNil]): Aux[L, HNil, N, HNil] = new Pointers0[L, HNil, N] {
    type Out = HNil

    def get = HNil
  }

  implicit def hconsPointers[L <: HList, N <: Nat, H, T <: HList](implicit
    d: Drop.Aux[L, N, H :: T],
    n: ToInt[N],
    tp: Pointers0[L, T, Succ[N]]
  ): Aux[L, H :: T, N, Ptr.Aux[L, N, H] :: tp.Out] = new Pointers0[L, H :: T, N] {
    type Out = Ptr.Aux[L, N, H] :: tp.Out

    def get = new Ptr[L, N] {
      type Out = H

      def index: Int = n()

      def apply(l: L) = d(l).head
    } :: tp.get
  }
}