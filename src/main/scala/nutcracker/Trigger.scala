package nutcracker

import nutcracker.util.{FreeK}

import scala.language.higherKinds
import scalaz.{Functor, ~>}

sealed trait Trigger[K]
case class Discard[K]() extends Trigger[K]
case class Sleep[K]() extends Trigger[K]
case class Fire[K](cont: K) extends Trigger[K]
case class FireReload[K](cont: K) extends Trigger[K]

object Trigger {
  def discard[F[_[_], _]]: Trigger[FreeK[F, Unit]] = Discard[FreeK[F, Unit]]()
  def sleep[F[_[_], _]]: Trigger[FreeK[F, Unit]] = Sleep[FreeK[F, Unit]]()
  def fire[F[_[_], _]](cont: FreeK[F, Unit]): Trigger[FreeK[F, Unit]] = Fire[FreeK[F, Unit]](cont)
  def fireReload[F[_[_], _]](cont: FreeK[F, Unit]): Trigger[FreeK[F, Unit]] = FireReload[FreeK[F, Unit]](cont)

  implicit def functorInstance: Functor[Trigger] = new Functor[Trigger] {
    def map[K, L](tk: Trigger[K])(f: K => L): Trigger[L] = tk match {
      case Discard() => Discard()
      case Sleep() => Sleep()
      case Fire(k) => Fire(f(k))
      case FireReload(k) => FireReload(f(k))
    }
  }
}
