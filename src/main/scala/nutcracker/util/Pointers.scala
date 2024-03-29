package nutcracker.util

import nutcracker.util.HList.{::, Drop, HNil}
import nutcracker.util.Nat.{_0, Succ}

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
    ptr: HListPtr.Aux[L, N, H],
    tp: Pointers0[L, T, Succ[N]]
  ): Aux[L, H :: T, N, HListPtr.Aux[L, N, H] :: tp.Out] = new Pointers0[L, H :: T, N] {
    type Out = HListPtr.Aux[L, N, H] :: tp.Out

    def get = ptr :: tp.get
  }
}