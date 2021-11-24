package nutcracker.util

import nutcracker.util.Nat.{Succ, _0}

sealed trait HList {
  def ::[H](h: H): HList.::[H, this.type] =
    HList.::(h, this)
}

object HList {
  object HNil extends HList
  type HNil = HNil.type

  case class ::[+H, +T <: HList](head: H, tail: T) extends HList

  sealed trait Length[L <: HList] {
    type Out <: Nat
  }

  object Length {
    type Aux[L <: HList, N <: Nat] = Length[L] { type Out = N }

    implicit val nilLength: Length.Aux[HNil, _0] =
      new Length[HNil] {
        type Out = _0
      }

    implicit def consLength[H, T <: HList, N <: Nat](implicit
      tailLength: Length.Aux[T, N],
    ): Length.Aux[H :: T, Succ[N]] =
      new Length[H :: T] {
        type Out = Succ[N]
      }
  }

  sealed trait Drop[L <: HList, N <: Nat] {
    type Out <: HList
  }

  object Drop {
    type Aux[L <: HList, N <: Nat, L0 <: HList] = Drop[L, N] { type Out = L0 }

    implicit def drop0[L <: HList]: Drop.Aux[L, _0, L] =
      new Drop[L, _0] {
        type Out = L
      }

    implicit def dropSucc[H, T <: HList, N <: Nat, T0 <: HList](implicit
      dropTail: Drop.Aux[T, N, T0],
    ): Drop.Aux[H :: T, Succ[N], T0] =
      new Drop[H :: T, Succ[N]] {
        type Out = T0
      }
  }
}
