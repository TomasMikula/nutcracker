package nutcracker.util

import nutcracker.util.HList.{::, HNil}

/**
  * Used to implicitly summon an HList by implicitly summoning its elements.
  */
trait SummonHList[L <: HList] {
  def get: L

  def ::[H](h: H): SummonHList[H :: L] =
    new SummonHList[H :: L] {
      override def get: H :: L = h :: SummonHList.this.get
    }
}

object SummonHList {
  implicit def hnilWrapper: SummonHList[HNil] = new SummonHList[HNil] {
    def get: HNil = HNil
  }

  implicit def hconsWrapper[H, T <: HList](implicit h: H, w: SummonHList[T]): SummonHList[H :: T] =
    h :: w
}