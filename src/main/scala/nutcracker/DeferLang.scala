package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, InjectK}

sealed trait DeferLang[D, K[_], A]

object DeferLang {
  final case class Defer[D, K[_]](delay: D, k: K[Unit]) extends DeferLang[D, K, Unit]

  def defer[D, K[_]](delay: D, k: K[Unit]): DeferLang[D, K, Unit] =
    Defer(delay, k)

  def deferF[D, F[_[_], _]](delay: D, k: FreeK[F, Unit])(implicit inj: InjectK[DeferLang[D, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[DeferLang[D, ?[_], ?], F, Unit](defer[D, FreeK[F, ?]](delay, k))
}