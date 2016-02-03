package nutcracker.util

import shapeless._

trait Choose[L <: HList, C <: HList] extends (L => C) {
  def vertexSet: Set[Int]

  def ::[N <: Nat, A](n: N)(implicit ptr: Ptr.Aux[L, n.N, A]): Choose[L, A :: C] = new Choose[L, A :: C] { // linter:ignore UnusedParameter // argument n is there just to infer N
    override lazy val vertexSet: Set[Int] = Choose.this.vertexSet + ptr.index
    def apply(l: L): (A :: C) = ptr(l) :: Choose.this.apply(l)
  }

  def ::[N <: Nat, A](ptr: Ptr.Aux[L, N, A]): Choose[L, A :: C] = new Choose[L, A :: C] {
    override lazy val vertexSet: Set[Int] = Choose.this.vertexSet + ptr.index
    def apply(l: L): (A :: C) = ptr(l) :: Choose.this.apply(l)
  }
}

object Choose {

  def apply[L <: HList]: Choose[L, HNil] = new Choose[L, HNil] {
    def vertexSet: Set[Int] = Set.empty
    def apply(v1: L): HNil = HNil
  }
}


trait ChooseByPtrs[L <: HList, C <: HList, Ptrs <: HList] extends (Ptrs => Choose[L, C])

object ChooseByPtrs {

  implicit def chooseHNil[L <: HList]: ChooseByPtrs[L, HNil, HNil] = new ChooseByPtrs[L, HNil, HNil] {
    def apply(ptrs: HNil): Choose[L, HNil] = Choose[L]
  }

  implicit def chooseHCons[L <: HList, H, T <: HList, PT <: HList, N <: Nat](implicit ch: ChooseByPtrs[L, T, PT]): ChooseByPtrs[L, H :: T, Ptr.Aux[L, N, H] :: PT] =
    new ChooseByPtrs[L, H :: T, Ptr.Aux[L, N, H] :: PT] {
      def apply(ptrs: Ptr.Aux[L, N, H] :: PT): Choose[L, H :: T] = ptrs.head :: ch(ptrs.tail)
    }
}