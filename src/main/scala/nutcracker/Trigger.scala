package nutcracker

import scala.language.higherKinds
import scalaz.Functor

sealed trait Trigger[K]
case class Discard[K]() extends Trigger[K]
case class Sleep[K]() extends Trigger[K]
case class Fire[K](cont: K) extends Trigger[K]
case class FireReload[K](cont: K) extends Trigger[K]

object Trigger {
  def discard[F[_]]: Trigger[F[Unit]] = Discard()
  def sleep[F[_]]: Trigger[F[Unit]] = Sleep()
  def fire[F[_]](cont: F[Unit]): Trigger[F[Unit]] = Fire(cont)
  def fireReload[F[_]](cont: F[Unit]): Trigger[F[Unit]] = FireReload(cont)

  implicit def functorInstance: Functor[Trigger] = new Functor[Trigger] {
    def map[K, L](tk: Trigger[K])(f: K => L): Trigger[L] = tk match {
      case Discard() => Discard()
      case Sleep() => Sleep()
      case Fire(k) => Fire(f(k))
      case FireReload(k) => FireReload(f(k))
    }
  }
}
