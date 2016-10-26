package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK}
import scalaz.~>

sealed trait DeferLang[D, K[_], A]

object DeferLang {
  final case class Delay[D, K[_]](delay: D, k: K[Unit]) extends DeferLang[D, K, Unit]

  def defer[D, K[_]](delay: D, k: K[Unit]): DeferLang[D, K, Unit] =
    Delay(delay, k)

  implicit def functorKAInstance[D]: FunctorKA[DeferLang[D, ?[_], ?]] = new FunctorKA[DeferLang[D, ?[_], ?]] {
    def transform[K[_], L[_], A](d: DeferLang[D, K, A])(f: K ~> L): DeferLang[D, L, A] = d match {
      case Delay(d, k) => Delay(d, f(k))
    }
  }

  implicit def deferInstance[F[_[_], _], D](implicit inj: InjectK[DeferLang[D, ?[_], ?], F]): Defer[FreeK[F, ?], D] =
    new Defer[FreeK[F, ?], D] {
      def defer(delay: D, k: FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.injLiftF(DeferLang.defer[D, FreeK[F, ?]](delay, k))
    }
}