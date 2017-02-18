package nutcracker.util

import scala.language.higherKinds
import scalaz.{Lens, StateT}
import scalaz.std.option._

final case class Uncons[S[_[_]]](run: `Forall{(* -> *) -> *}`[λ[K[_] => StateT[Option, S[K], Lst[K[Unit]]]]]) extends AnyVal { self =>
  def apply[K[_]] = run[K]

  def zoomOut[T[_[_]]](implicit f: `Forall{(* -> *) -> *}`[λ[K[_] => Lens[T[K], S[K]]]]): Uncons[T] = {
    type StS[K[_]] = StateT[Option, S[K], Lst[K[Unit]]]
    type StT[K[_]] = StateT[Option, T[K], Lst[K[Unit]]]
    Uncons[T](run.transform[StT](new (StS ≈> StT) {
      def apply[K[_]](sts: StS[K]): StT[K] = sts.zoom(f[K])
    }))
  }

  def orElse(that: Uncons[S]): Uncons[S] = Uncons[S](new `Forall{(* -> *) -> *}`[λ[K[_] => StateT[Option, S[K], Lst[K[Unit]]]]] {
    override def compute[K[_]]: StateT[Option, S[K], Lst[K[Unit]]] =
      StateT(s => self[K](s).orElse(that[K](s)))
  })
}
