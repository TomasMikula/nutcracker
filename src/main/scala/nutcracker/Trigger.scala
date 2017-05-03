package nutcracker

import scalaz.Functor

sealed trait TriggerF[F[_], D, Δ, A] {
  import TriggerF._

  def map[B](f: A => B)(implicit F: Functor[F]): TriggerF[F, D, Δ, B] = this match {
    case Discard() => Discard()
    case Fire(exec) => Fire(exec)
    case Sleep(next) => Sleep((d, δ) => f(next(d, δ)))
    case FireReload(exec, next) => FireReload(exec, (d, δ) => f(next(d, δ)))
    case Reconsider(fa) => Reconsider(F.map(fa)(f))
  }

  def contramap[D0, Δ0](f: D0 => D, g: Δ0 => Δ): TriggerF[F, D0, Δ0, A] = this match {
    case Discard() => Discard()
    case Sleep(next) => Sleep((d0, δ0) => next(f(d0), g(δ0)))
    case Fire(exec) => Fire(exec)
    case FireReload(exec, next) => FireReload(exec, (d0, δ0) => next(f(d0), g(δ0)))
    case Reconsider(cont) => Reconsider(cont)
  }
}

object TriggerF {

  case class Discard[F[_], D, Δ, A]() extends TriggerF[F, D, Δ, A]
  case class Fire[F[_], D, Δ, A](exec: F[Unit]) extends TriggerF[F, D, Δ, A]
  case class Sleep[F[_], D, Δ, A](next: (D, Δ) => A) extends TriggerF[F, D, Δ, A]
  case class FireReload[F[_], D, Δ, A](exec: F[Unit], next: (D, Δ) => A) extends TriggerF[F, D, Δ, A]
  case class Reconsider[F[_], D, Δ, A](cont: F[A]) extends TriggerF[F, D, Δ, A]
}

private[nutcracker] sealed trait SeqTrigger[Tok[_], K[_], D, Δ[_, _], D1] {
  import SeqTrigger._

  def specialize[D2 <: D1]: SeqTrigger[Tok, K, D, Δ, D2] = this.asInstanceOf[SeqTrigger[Tok, K, D, Δ, D2]]

  def fold[B](
    caseDiscard: => B,
    caseSleep: SeqHandler[Tok, K, D, Δ, D1] => B,
    caseFire: K[Unit] => B,
    caseFireReload: (K[Unit], SeqHandler[Tok, K, D, Δ, D1]) => B,
    caseReconsider: (Tok[D1] => K[Unit]) => B
  ): B = this match {
    case Discard() => caseDiscard
    case Sleep(h) => caseSleep(h)
    case Fire(k) => caseFire(k)
    case FireReload(k, h) => caseFireReload(k, h)
    case Reconsider(f) => caseReconsider(f)
  }
}

private[nutcracker] object SeqTrigger {

  case class Discard[Tok[_], K[_], D, Δ[_, _], D1]() extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Sleep[Tok[_], K[_], D, Δ[_, _], D1](h: SeqHandler[Tok, K, D, Δ, D1]) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Fire[Tok[_], K[_], D, Δ[_, _], D1](cont: K[Unit]) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class FireReload[Tok[_], K[_], D, Δ[_, _], D1](cont: K[Unit], h: SeqHandler[Tok, K, D, Δ, D1]) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Reconsider[Tok[_], K[_], D, Δ[_, _], D1](cont: Tok[D1] => K[Unit]) extends SeqTrigger[Tok, K, D, Δ, D1]

}

private[nutcracker] trait SeqHandler[Tok[_], K[_], D, Δ[_, _], D1] { self =>
  def handle[D2 <: D](d2: D2, δ: Δ[D1, D2]): SeqTrigger[Tok, K, D, Δ, D2]

  def specialize[D2 <: D1]: SeqHandler[Tok, K, D, Δ, D2] = this.asInstanceOf[SeqHandler[Tok, K, D, Δ, D2]]
}

private[nutcracker] object SeqHandler {
  def apply[Tok[_], K[_], D, Δ](h: (D, Δ) => SeqTrigger[Tok, K, D, λ[(α, β) => Δ], D]): SeqHandler[Tok, K, D, λ[(α, β) => Δ], D] =
    new SeqHandler[Tok, K, D, λ[(α, β) => Δ], D] {
      override def handle[D2 <: D](d2: D2, δ: Δ): SeqTrigger[Tok, K, D, λ[(α, β) => Δ], D2] = h(d2, δ).specialize[D2]
    }
}

private[nutcracker] trait SeqPreHandler[Tok[_], K[_], D, Δ[_, _]] { self =>
  def handle[D0 <: D](d: D0): SeqTrigger[Tok, K, D, Δ, D0]
}

private[nutcracker] object SeqPreHandler {
  def apply[Tok[_], K[_], D, Δ[_, _]](h: D => SeqTrigger[Tok, K, D, Δ, D]): SeqPreHandler[Tok, K, D, Δ] = new SeqPreHandler[Tok, K, D, Δ] {
    def handle[D0 <: D](d: D0): SeqTrigger[Tok, K, D, Δ, D0] = h(d).specialize[D0]
  }
}