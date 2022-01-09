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

private[nutcracker] sealed trait SeqTrigger[Tok[_], K[_], D[_], Δ[_, _], I] {
  import SeqTrigger._

  def fold[B](
    caseDiscard: => B,
    caseSleep: SeqHandler[Tok, K, D, Δ, I] => B,
    caseFire: K[Unit] => B,
    caseFireReload: (K[Unit], SeqHandler[Tok, K, D, Δ, I]) => B,
    caseReconsider: (Tok[I] => K[Unit]) => B
  ): B = this match {
    case Discard() => caseDiscard
    case Sleep(h) => caseSleep(h)
    case Fire(k) => caseFire(k)
    case FireReload(k, h) => caseFireReload(k, h)
    case Reconsider(f) => caseReconsider(f)
  }
}

private[nutcracker] object SeqTrigger {

  case class Discard[Tok[_], K[_], D[_], Δ[_, _], J]() extends SeqTrigger[Tok, K, D, Δ, J]

  case class Sleep[Tok[_], K[_], D[_], Δ[_, _], J](h: SeqHandler[Tok, K, D, Δ, J]) extends SeqTrigger[Tok, K, D, Δ, J]

  case class Fire[Tok[_], K[_], D[_], Δ[_, _], J](cont: K[Unit]) extends SeqTrigger[Tok, K, D, Δ, J]

  case class FireReload[Tok[_], K[_], D[_], Δ[_, _], J](cont: K[Unit], h: SeqHandler[Tok, K, D, Δ, J]) extends SeqTrigger[Tok, K, D, Δ, J]

  case class Reconsider[Tok[_], K[_], D[_], Δ[_, _], J](cont: Tok[J] => K[Unit]) extends SeqTrigger[Tok, K, D, Δ, J]

}

private[nutcracker] trait SeqHandler[Tok[_], K[_], D[_], Δ[_, _], I] { self =>
  def handle[J](d2: D[J], δ: Δ[I, J]): SeqTrigger[Tok, K, D, Δ, J]
}

private[nutcracker] object SeqHandler {
  def apply[Tok[_], K[_], D[_], Δ[_, _], I](h: [j] => (D[j], Δ[I, j]) => SeqTrigger[Tok, K, D, Δ, j]): SeqHandler[Tok, K, D, Δ, I] =
    new SeqHandler[Tok, K, D, Δ, I] {
      override def handle[J](d2: D[J], δ: Δ[I, J]): SeqTrigger[Tok, K, D, Δ, J] = h[J](d2, δ)
    }
}

private[nutcracker] trait SeqPreHandler[Tok[_], K[_], D[_], Δ[_, _]] { self =>
  def handle[I](d: D[I]): SeqTrigger[Tok, K, D, Δ, I]
}

private[nutcracker] object SeqPreHandler {
  def apply[Tok[_], K[_], D[_], Δ[_, _]](h: [i] => D[i] => SeqTrigger[Tok, K, D, Δ, i]): SeqPreHandler[Tok, K, D, Δ] = new SeqPreHandler[Tok, K, D, Δ] {
    override def handle[I](d: D[I]): SeqTrigger[Tok, K, D, Δ, I] = h[I](d)
  }
}