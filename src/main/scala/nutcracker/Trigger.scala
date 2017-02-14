package nutcracker

import scala.language.higherKinds
import scalaz.Functor

sealed trait Trigger[K]

object Trigger {
  case class Discard[K]() extends Trigger[K]
  case class Sleep[K]() extends Trigger[K]
  case class Fire[K](cont: K) extends Trigger[K]
  case class FireReload[K](cont: K) extends Trigger[K]

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

sealed trait SeqTrigger[Token[_], K, D, Δ[_, _], D1] {
  import SeqTrigger._

  def map[L](f: K => L): SeqTrigger[Token, L, D, Δ, D1] = this match {
    case Discard() => Discard()
    case Sleep(h) => Sleep(h.map(f))
    case Fire(k) => Fire(f(k))
    case FireReload(cont) => FireReload(cont andThen f)
  }
}
object SeqTrigger {
  case class Discard[Token[_], K, D, Δ[_, _], D1]() extends SeqTrigger[Token, K, D, Δ, D1]
  case class Sleep[Token[_], K, D, Δ[_, _], D1](h: SeqHandler[Token, K, D, Δ, D1]) extends SeqTrigger[Token, K, D, Δ, D1]
  case class Fire[Token[_], K, D, Δ[_, _], D1](cont: K) extends SeqTrigger[Token, K, D, Δ, D1]
  case class FireReload[Token[_], K, D, Δ[_, _], D1](cont: Token[D1] => K) extends SeqTrigger[Token, K, D, Δ, D1]
}

trait SeqHandler[Token[_], K, D, Δ[_, _], D1] { self =>
  def handle[D2 <: D](d2: D2, δ: Δ[D1, D2]): SeqTrigger[Token, K, D, Δ, D2]

  def map[L](f: K => L): SeqHandler[Token, L, D, Δ, D1] = new SeqHandler[Token, L, D, Δ, D1] {
    def handle[D2 <: D](d2: D2, δ: Δ[D1, D2]): SeqTrigger[Token, L, D, Δ, D2] =
      self.handle(d2, δ).map(f)
  }
}

trait SeqPreHandler[Token[_], K, D, Δ[_, _]] { self =>
  def handle[D0 <: D](d: D0): (Option[K], Option[SeqHandler[Token, K, D, Δ, D0]])

  def map[L](f: K => L): SeqPreHandler[Token, L, D, Δ] = new SeqPreHandler[Token, L, D, Δ] {
    def handle[D0 <: D](d: D0): (Option[L], Option[SeqHandler[Token, L, D, Δ, D0]]) = {
      val (now, later) = self.handle(d)
      (now.map(f), later.map(_ map f))
    }
  }
}