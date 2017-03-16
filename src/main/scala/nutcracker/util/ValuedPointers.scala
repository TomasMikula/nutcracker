package nutcracker.util

import shapeless._

trait ValuedPointers[V <: HList, PA <: HList] {
  type Values <: HList
  def apply(pa: PA): (Choose[V, Values], Values)
}

object ValuedPointers {
  implicit def hnilPointers[V <: HList]: ValuedPointers[V, HNil] = new ValuedPointers[V, HNil] {
    type Values = HNil
    def apply(pa: HNil): (Choose[V, HNil], HNil) = (Choose[V], HNil)
  }

  implicit def hconsPointers[V <: HList, N <: Nat, H, PT <: HList](implicit vp: ValuedPointers[V, PT]): ValuedPointers[V, (HListPtr.Aux[V, N, H], H) :: PT] =
    new ValuedPointers[V, (HListPtr.Aux[V, N, H], H) :: PT] {
      type Values = H :: vp.Values
      def apply(pa: (HListPtr.Aux[V, N, H], H) :: PT): (Choose[V, Values], Values) = {
        val (ch, c) = vp(pa.tail)
        (pa.head._1 :: ch, pa.head._2 :: c)
      }
    }
}