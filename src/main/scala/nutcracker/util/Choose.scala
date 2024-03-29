package nutcracker.util

import nutcracker.util.HList.{::, HNil}
import scala.language.implicitConversions

trait Choose[L <: HList, C <: HList] extends (L => C) {
  def get(l: L): C
  def set(l: L, c: C): L
  def vertices: List[Int]
  def vertexSet: Set[Int]

  final def apply(l: L): C = get(l)

  def ::[N <: Nat, A](n: N)(implicit ptr: HListPtr.Aux[L, n.N, A]): Choose[L, A :: C] = // linter:ignore UnusedParameter // argument n is there just to infer N
    ptr :: this

  def ::[N <: Nat, A](ptr: HListPtr.Aux[L, N, A]): Choose[L, A :: C] = new Choose[L, A :: C] {
    override lazy val vertexSet: Set[Int] = Choose.this.vertexSet + ptr.index
    override lazy val vertices: List[Int] = ptr.index :: Choose.this.vertices
    def get(l: L): (A :: C) = ptr(l) :: Choose.this.apply(l)
    def set(l: L, ac: A :: C): L = ptr.set(Choose.this.set(l, ac.tail), ac.head)
  }

  def lift[F[_]](implicit ml: Mapped[L, F], mc: Mapped[C, F]): Choose[ml.Out, mc.Out] = this.asInstanceOf[Choose[ml.Out, mc.Out]] // cheating, but screw it

  trait Lifter[F[_]] {
    def apply[FL <: HList, FC <: HList]()(implicit ml: Mapped.Aux[L, F, FL], mc: Mapped.Aux[C, F, FC]): Choose[FL, FC]
  }

  final override def equals(other: Any): Boolean = other match {
    case that: Choose[_, _] => that.vertices == this.vertices
    case _ => false
  }

  final override def hashCode: Int = vertices.hashCode
}

object Choose {

  def apply[L <: HList]: Choose[L, HNil] = new Choose[L, HNil] {
    def vertexSet: Set[Int] = Set.empty
    def vertices: List[Int] = Nil
    def get(v1: L): HNil = HNil
    def set(l: L, c: HNil): L = l
  }

  implicit def chooseByNats[L <: HList, C <: HList, NS <: HList](ns: NS)(implicit ch: ChooseByNats[L, C, NS]): Choose[L, C] = ch(ns)
}


trait ChooseByPtrs[L <: HList, C <: HList, Ptrs <: HList] extends (Ptrs => Choose[L, C])

object ChooseByPtrs {

  implicit def chooseHNil[L <: HList]: ChooseByPtrs[L, HNil, HNil] = new ChooseByPtrs[L, HNil, HNil] {
    def apply(ptrs: HNil): Choose[L, HNil] = Choose[L]
  }

  implicit def chooseHCons[L <: HList, H, T <: HList, PT <: HList, N <: Nat](implicit ch: ChooseByPtrs[L, T, PT]): ChooseByPtrs[L, H :: T, HListPtr.Aux[L, N, H] :: PT] =
    new ChooseByPtrs[L, H :: T, HListPtr.Aux[L, N, H] :: PT] {
      def apply(ptrs: HListPtr.Aux[L, N, H] :: PT): Choose[L, H :: T] = ptrs.head :: ch(ptrs.tail)
    }
}

trait ChooseByNats[L <: HList, C <: HList, NS <: HList] extends (NS => Choose[L, C])

object ChooseByNats {
  implicit def chooseHNil[L <: HList]: ChooseByNats[L, HNil, HNil] = new ChooseByNats[L, HNil, HNil] {
    def apply(nats: HNil): Choose[L, HNil] = Choose[L]
  }

  implicit def chooseHCons[L <: HList, H, T <: HList, N <: Nat, NS <: HList](implicit
    ch: ChooseByNats[L, T, NS],
    ptr: HListPtr.Aux[L, N, H]
  ): ChooseByNats[L, H :: T, N :: NS] = new ChooseByNats[L, H :: T, N :: NS] {
    def apply(nats: N :: NS): Choose[L, H :: T] = ptr :: ch(nats.tail)
  }
}