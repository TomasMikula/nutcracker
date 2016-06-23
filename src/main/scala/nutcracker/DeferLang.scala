package nutcracker

import scala.language.higherKinds
import nutcracker.util.{ContF, FreeK, FunctorKA, InjectK}
import scalaz.~>

sealed trait DeferLang[D, K[_], A]

object DeferLang {
  final case class Defer[D, K[_]](delay: D, k: K[Unit]) extends DeferLang[D, K, Unit]

  def defer[D, K[_]](delay: D, k: K[Unit]): DeferLang[D, K, Unit] =
    Defer(delay, k)

  def deferF[D, F[_[_], _]](delay: D, k: FreeK[F, Unit])(implicit inj: InjectK[DeferLang[D, ?[_], ?], F]): FreeK[F, Unit] =
    FreeK.injLiftF[DeferLang[D, ?[_], ?], F, Unit](defer[D, FreeK[F, ?]](delay, k))

  /** Defer registration of callbacks to the given CPS computation. */
  def deferC[D, F[_[_], _], A](delay: D, c: ContF[F, A])(implicit inj: InjectK[DeferLang[D, ?[_], ?], F]): ContF[F, A] =
    ContF(f => deferF(delay, c(f)))

  implicit def functorKAInstance[D]: FunctorKA[DeferLang[D, ?[_], ?]] = new FunctorKA[DeferLang[D, ?[_], ?]] {
    def transform[K[_], L[_], A](d: DeferLang[D, K, A])(f: K ~> L): DeferLang[D, L, A] = d match {
      case Defer(d, k) => Defer(d, f(k))
    }
  }
}
