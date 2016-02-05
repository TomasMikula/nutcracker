package nutcracker.util

import scala.language.higherKinds

import shapeless._

trait Choose[L <: HList, C <: HList] extends (L => C) {
  def vertices: List[Int]
  def vertexSet: Set[Int]

  def ::[N <: Nat, A](n: N)(implicit ptr: Ptr.Aux[L, n.N, A]): Choose[L, A :: C] = // linter:ignore UnusedParameter // argument n is there just to infer N
    ptr :: this

  def ::[N <: Nat, A](ptr: Ptr.Aux[L, N, A]): Choose[L, A :: C] = new Choose[L, A :: C] {
    override lazy val vertexSet: Set[Int] = Choose.this.vertexSet + ptr.index
    override lazy val vertices: List[Int] = ptr.index :: Choose.this.vertices
    def apply(l: L): (A :: C) = ptr(l) :: Choose.this.apply(l)
  }

  def lift[F[_]](implicit ml: Mapped[L, F], mc: Mapped[C, F]): Choose[ml.Out, mc.Out] = this.asInstanceOf[Choose[ml.Out, mc.Out]] // cheating, but screw it

  trait Lifter[F[_]] {
    def apply[FL <: HList, FC <: HList]()(implicit ml: Mapped.Aux[L, F, FL], mc: Mapped.Aux[C, F, FC]): Choose[FL, FC]
  }
}

object Choose {

  def apply[L <: HList]: Choose[L, HNil] = new Choose[L, HNil] {
    def vertexSet: Set[Int] = Set.empty
    def vertices: List[Int] = Nil
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