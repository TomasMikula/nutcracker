package nutcracker.rel

import nutcracker.util.ChooseByPtrs
import shapeless._

trait Rel[L <: HList] { self: Singleton =>

  def apply[V <: HList, Ptrs <: HList](
    ptrs: Ptrs)(implicit
    ch: ChooseByPtrs[V, L, Ptrs]
  ): RelChoice[V, L] =
    RelChoice(this, ch(ptrs))

  def apply[V <: HList, A1]                    (a1 : A1)                                        (implicit ch: ChooseByPtrs[V, L, A1 :: HNil]):                               RelChoice[V, L] = apply(a1 :: HNil)
  def apply[V <: HList, A1, A2]                (a1 : A1, a2: A2)                                (implicit ch: ChooseByPtrs[V, L, A1 :: A2 :: HNil]):                         RelChoice[V, L] = apply(a1 :: a2 :: HNil)
  def apply[V <: HList, A1, A2, A3]            (a1 : A1, a2: A2, a3: A3)                        (implicit ch: ChooseByPtrs[V, L, A1 :: A2 :: A3 :: HNil]):                   RelChoice[V, L] = apply(a1 :: a2 :: a3 :: HNil)
  def apply[V <: HList, A1, A2, A3, A4]        (a1 : A1, a2: A2, a3: A3, a4: A4)                (implicit ch: ChooseByPtrs[V, L, A1 :: A2 :: A3 :: A4 :: HNil]):             RelChoice[V, L] = apply(a1 :: a2 :: a3 :: a4 :: HNil)
  def apply[V <: HList, A1, A2, A3, A4, A5]    (a1 : A1, a2: A2, a3: A3, a4: A4, a5: A5)        (implicit ch: ChooseByPtrs[V, L, A1 :: A2 :: A3 :: A4 :: A5 :: HNil]):       RelChoice[V, L] = apply(a1 :: a2 :: a3 :: a4 :: a5 :: HNil)
  def apply[V <: HList, A1, A2, A3, A4, A5, A6](a1 : A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(implicit ch: ChooseByPtrs[V, L, A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil]): RelChoice[V, L] = apply(a1 :: a2 :: a3 :: a4 :: a5 :: a6 :: HNil)
}

object Rel {
  type Rel1[A1] = Rel[A1 :: HNil]
  type Rel2[A1, A2] = Rel[A1 :: A2 :: HNil]
  type Rel3[A1, A2, A3] = Rel[A1 :: A2 :: A3 :: HNil]
  type Rel4[A1, A2, A3, A4] = Rel[A1 :: A2 :: A3 :: A4 :: HNil]
  type Rel5[A1, A2, A3, A4, A5] = Rel[A1 :: A2 :: A3 :: A4 :: A5 :: HNil]
  type Rel6[A1, A2, A3, A4, A5, A6] = Rel[A1 :: A2 :: A3 :: A4 :: A5 :: A6 :: HNil]
}