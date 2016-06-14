package nutcracker.util

import scala.language.higherKinds
import scalaz.{Lens, StateT, ~>}
import scalaz.std.option._

final case class Uncons[S[_]](run: ValA[λ[K => StateT[Option, S[K], Lst[K]]]]) extends AnyVal { self =>
  def apply[K] = run[K]

  def zoomOut[T[_]](f: ValA[λ[K => Lens[T[K], S[K]]]]): Uncons[T] = {
    type StS[K] = StateT[Option, S[K], Lst[K]]
    type StT[K] = StateT[Option, T[K], Lst[K]]
    Uncons[T](run.transform[StT](new (StS ~> StT) {
      def apply[K](sts: StS[K]): StT[K] = sts.zoom(f[K])
    }))
  }

  def orElse(that: Uncons[S]): Uncons[S] = Uncons[S](new ValA[λ[K => StateT[Option, S[K], Lst[K]]]] {
    override def compute[K]: StateT[Option, S[K], Lst[K]] =
      StateT(s => self[K](s).orElse(that[K](s)))
  })
}
