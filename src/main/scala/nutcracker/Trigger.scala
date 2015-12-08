package nutcracker

import nutcracker.util.free.{FreeK, FunctorK}

import scala.language.higherKinds
import scalaz.~>

sealed trait Trigger[K[_]]
case class Discard[K[_]]() extends Trigger[K]
case class Sleep[K[_]]() extends Trigger[K]
case class Fire[K[_]](cont: K[Unit]) extends Trigger[K]
case class FireReload[K[_]](cont: K[Unit]) extends Trigger[K]

object Trigger {
  def discard[F[_[_], _]]: Trigger[FreeK[F, ?]] = Discard[FreeK[F, ?]]()
  def sleep[F[_[_], _]]: Trigger[FreeK[F, ?]] = Sleep[FreeK[F, ?]]()
  def fire[F[_[_], _]](cont: FreeK[F, Unit]): Trigger[FreeK[F, ?]] = Fire[FreeK[F, ?]](cont)
  def fireReload[F[_[_], _]](cont: FreeK[F, Unit]): Trigger[FreeK[F, ?]] = FireReload[FreeK[F, ?]](cont)

  implicit def functorKInstance: FunctorK[Trigger] = new FunctorK[Trigger] {
    def transform[K[_], L[_]](tk: Trigger[K])(f: K ~> L): Trigger[L] = tk match {
      case Discard() => Discard()
      case Sleep() => Sleep()
      case Fire(k) => Fire(f(k))
      case FireReload(k) => FireReload(f(k))
    }
  }
}