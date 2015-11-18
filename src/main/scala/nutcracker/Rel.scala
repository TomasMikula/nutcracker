package nutcracker

import shapeless._

trait Rel[L <: HList]

object Rel {
  type Rel1[A1] = Rel[A1 :: HNil]
  type Rel2[A1, A2] = Rel[A1 :: A2 :: HNil]
  type Rel3[A1, A2, A3] = Rel[A1 :: A2 :: A3 :: HNil]
  type Rel4[A1, A2, A3, A4] = Rel[A1 :: A2 :: A3 :: A4 :: HNil]
}