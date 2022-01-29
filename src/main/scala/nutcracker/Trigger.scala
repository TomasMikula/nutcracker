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

private[nutcracker] sealed trait SeqTrigger[K[_], D[_], Δ[_, _], I] {
  import SeqTrigger._

  def fold[B](
    caseDiscard: => B,
    caseSleep: SeqHandler[K, D, Δ, I] => B,
    caseFire: K[Unit] => B,
    caseFireReload: (K[Unit], SeqHandler[K, D, Δ, I]) => B,
    caseReconsider: ((SeqTrigger[K, D, Δ, I] => K[Unit]) => K[Unit]) => B
  ): B = this match {
    case Discard() => caseDiscard
    case Sleep(h) => caseSleep(h)
    case Fire(k) => caseFire(k)
    case FireReload(k, h) => caseFireReload(k, h)
    case Reconsider(f) => caseReconsider(f)
  }
}

private[nutcracker] object SeqTrigger {

  case class Discard[K[_], D[_], Δ[_, _], J]() extends SeqTrigger[K, D, Δ, J]

  case class Sleep[K[_], D[_], Δ[_, _], J](h: SeqHandler[K, D, Δ, J]) extends SeqTrigger[K, D, Δ, J]

  case class Fire[K[_], D[_], Δ[_, _], J](action: K[Unit]) extends SeqTrigger[K, D, Δ, J]

  case class FireReload[K[_], D[_], Δ[_, _], J](action: K[Unit], h: SeqHandler[K, D, Δ, J]) extends SeqTrigger[K, D, Δ, J]

  case class Reconsider[K[_], D[_], Δ[_, _], J](cont: (SeqTrigger[K, D, Δ, J] => K[Unit]) => K[Unit]) extends SeqTrigger[K, D, Δ, J]

}

private[nutcracker] trait SeqHandler[K[_], D[_], Δ[_, _], I] { self =>
  def handle[J](d2: D[J], δ: Δ[I, J]): SeqTrigger[K, D, Δ, J]
}

private[nutcracker] object SeqHandler {
  def apply[K[_], D[_], Δ[_, _], I](h: [j] => (D[j], Δ[I, j]) => SeqTrigger[K, D, Δ, j]): SeqHandler[K, D, Δ, I] =
    new SeqHandler[K, D, Δ, I] {
      override def handle[J](d2: D[J], δ: Δ[I, J]): SeqTrigger[K, D, Δ, J] = h[J](d2, δ)
    }
}

private[nutcracker] trait SeqPreHandler[K[_], D[_], Δ[_, _]] { self =>
  def handle[I](d: D[I]): SeqTrigger[K, D, Δ, I]
}

private[nutcracker] object SeqPreHandler {
  def apply[K[_], D[_], Δ[_, _]](h: [i] => D[i] => SeqTrigger[K, D, Δ, i]): SeqPreHandler[K, D, Δ] = new SeqPreHandler[K, D, Δ] {
    override def handle[I](d: D[I]): SeqTrigger[K, D, Δ, I] = h[I](d)
  }
}