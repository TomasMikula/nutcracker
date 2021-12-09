package nutcracker.util

import nutcracker.util.HList.::
import nutcracker.util.Nat.{_0, Succ}
import scala.language.implicitConversions

sealed trait HListPtr[L <: HList, N <: Nat] {
  type Out

  def index: Int
  def get(l: L): Out
  def set(l: L, a: Out): L

  final def apply(l: L): Out = get(l)
  def lift[F[_]](implicit m: Mapped[L, F]): HListPtr.Aux[m.Out, N, F[Out]] = this.asInstanceOf[HListPtr.Aux[m.Out, N, F[Out]]] // cheating, but screw it
}

object HListPtr {
  type Aux[L <: HList, N <: Nat, Out0] = HListPtr[L, N] { type Out = Out0 }

  def apply[L <: HList, N <: Nat](implicit at: HListPtr[L, N]): Aux[L, N, at.Out] = at
  def apply[L <: HList, N <: Nat](n: N)(implicit at: HListPtr[L, N]): Aux[L, N, at.Out] = at // linter:ignore UnusedParameter

  implicit def hlistAtZero[H, T <: HList]: Aux[H :: T, _0, H] =
    new HListPtr[H :: T, _0] {
      type Out = H
      def get(l: H :: T): Out = l.head
      def set(l: H :: T, h: H): H :: T = h :: l.tail
      def index = 0
    }

  implicit def hlistAtN[H, T <: HList, N <: Nat](implicit
      att: HListPtr[T, N]): Aux[H :: T, Succ[N], att.Out] =
    new HListPtr[H :: T, Succ[N]] {
      type Out = att.Out
      def get(l: H :: T) : Out = att(l.tail)
      def set(l: H :: T, a: Out): H :: T = l.head :: att.set(l.tail, a)
      def index = att.index + 1
    }

  implicit def ptrFromNat[L <: HList, N <: Nat, A](n : N)(implicit ptr: HListPtr.Aux[L, N, A]): HListPtr.Aux[L, N, A] = ptr // linter:ignore UnusedParameter
}
