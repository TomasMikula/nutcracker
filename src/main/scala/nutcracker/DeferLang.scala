package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, InjectK}

sealed trait DeferLang[D, K[_], A]

object DeferLang {
  case class Delay[D, K[_]](delay: D, k: K[Unit]) extends DeferLang[D, K, Unit]

  def defer[D, K[_]](delay: D, k: K[Unit]): DeferLang[D, K, Unit] =
    Delay(delay, k)

  implicit def deferInstance[F[_[_], _], D](implicit inj: InjectK[DeferLang[D, ?[_], ?], F]): Defer[FreeK[F, ?], D] =
    new Defer[FreeK[F, ?], D] {
      def defer(delay: D, k: FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.injLiftF(DeferLang.defer[D, FreeK[F, ?]](delay, k))
    }
}