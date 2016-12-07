package nutcracker.util

import scala.language.higherKinds
import scala.language.implicitConversions

import shapeless._
import shapeless.ops.hlist.At

trait ListPtr[L <: HList, N <: Nat] extends At[L, N] {
  def index: Int
  def lift[F[_]](implicit m: Mapped[L, F]): ListPtr.Aux[m.Out, N, F[Out]] = this.asInstanceOf[ListPtr.Aux[m.Out, N, F[Out]]] // cheating, but screw it
}

object ListPtr {
  type Aux[L <: HList, N <: Nat, Out0] = ListPtr[L, N] { type Out = Out0 }

  def apply[L <: HList, N <: Nat](implicit at: ListPtr[L, N]): Aux[L, N, at.Out] = at
  def apply[L <: HList, N <: Nat](n: N)(implicit at: ListPtr[L, N]): Aux[L, N, at.Out] = at // linter:ignore UnusedParameter

  implicit def hlistAtZero[H, T <: HList]: Aux[H :: T, _0, H] =
    new ListPtr[H :: T, _0] {
      type Out = H
      def apply(l : H :: T): Out = l.head
      def index = 0
    }

  implicit def hlistAtN[H, T <: HList, N <: Nat](implicit
      att: ListPtr[T, N]): Aux[H :: T, Succ[N], att.Out] =
    new ListPtr[H :: T, Succ[N]] {
      type Out = att.Out
      def apply(l : H :: T) : Out = att(l.tail)
      def index = att.index + 1
    }

  implicit def ptrFromNat[L <: HList, N <: Nat, A](n : N)(implicit ptr: ListPtr.Aux[L, N, A]): ListPtr.Aux[L, N, A] = ptr // linter:ignore UnusedParameter
}
