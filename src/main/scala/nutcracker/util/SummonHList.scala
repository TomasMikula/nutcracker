package nutcracker.util

import shapeless._

/**
  * Used to implicitly summon an HList by implicitly summoning its elements.
  */
trait SummonHList[L <: HList] {
  def get: L
}

object SummonHList {
  implicit def hnilWrapper: SummonHList[HNil] = new SummonHList[HNil] {
    def get: HNil = HNil
  }

  implicit def hconsWrapper[H, T <: HList](implicit h: H, w: SummonHList[T]): SummonHList[H :: T] = new SummonHList[H :: T] {
    def get: H :: T = h :: w.get
  }
}