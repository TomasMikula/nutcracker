package nutcracker

import scalaz.Functor
import scalaz.syntax.functor._

sealed trait TriggerF[F[_], D, Δ, A] {
  import TriggerF._

  def map[B](f: A => B)(implicit F: Functor[F]): TriggerF[F, D, Δ, B] = this match {
    case Discard() => Discard()
    case Fire(exec) => Fire(exec)
    case Sleep(next) => Sleep((d, δ) => f(next(d, δ)))
    case Reconsider(fa) => Reconsider(F.map(fa)(f))
  }

  def contramap[D0, Δ0](f: D0 => D, g: Δ0 => Δ): TriggerF[F, D0, Δ0, A] = this match {
    case Discard() => Discard()
    case Sleep(next) => Sleep((d0, δ0) => next(f(d0), g(δ0)))
    case Fire(exec) => Fire(exec)
    case Reconsider(cont) => Reconsider(cont)
  }
}

object TriggerF {

  case class Discard[F[_], D, Δ, A]() extends TriggerF[F, D, Δ, A]
  case class Fire[F[_], D, Δ, A](exec: F[Unit]) extends TriggerF[F, D, Δ, A]
  case class Sleep[F[_], D, Δ, A](next: (D, Δ) => A) extends TriggerF[F, D, Δ, A]
  case class Reconsider[F[_], D, Δ, A](cont: F[A]) extends TriggerF[F, D, Δ, A]
}

final case class Trigger[F[_], D, Δ](unfix: TriggerF[F, D, Δ, Trigger[F, D, Δ]]) { // extends AnyVal { // https://issues.scala-lang.org/browse/SI-9600

  def contramap[D0, Δ0](f: D0 => D, g: Δ0 => Δ)(implicit F: Functor[F]): Trigger[F, D0, Δ0] =
    Trigger(unfix.contramap(f, g).map(_.contramap(f, g)))

}

object Trigger {
  import TriggerF._

  def discard[F[_], D, Δ]: Trigger[F, D, Δ] = Trigger(Discard())
  def sleep[F[_], D, Δ](next: (D, Δ) => Trigger[F, D, Δ]): Trigger[F, D, Δ] = Trigger(Sleep(next))
  def fire[F[_], D, Δ](exec: F[Unit]): Trigger[F, D, Δ] = Trigger(Fire(exec))
  def reconsider[F[_], D, Δ](cont: F[Trigger[F, D, Δ]]): Trigger[F, D, Δ] = Trigger(Reconsider(cont))

  def observerS[F[_]: Functor, D, Δ, S](s: S)(f: S => TriggerF[F, D, Δ, S]): Trigger[F, D, Δ] =
    Trigger(f(s) map (observerS(_)(f)))

  /** Keep trying `f` until it returns `Some`. Then fire the returned program. */
  def threshold[F[_], D, Δ](f: D => Option[F[Unit]]): D => Trigger[F, D, Δ] =
    d => f(d) match {
      case None => sleep(threshold1(f))
      case Some(k) => fire(k)
    }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program. */
  def threshold1[F[_], D, Δ](f: D => Option[F[Unit]]): (D, Δ) => Trigger[F, D, Δ] =
    new ((D, Δ) => Trigger[F, D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[F, D, Δ] = f(d) match {
        case None => sleep(this)
        case Some(k) => fire(k)
      }
    }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program, if any. */
  def thresholdOpt[F[_], D, Δ](f: D => Option[Option[F[Unit]]]): D => Trigger[F, D, Δ] =
    d => f(d) match {
      case None => sleep(thresholdOpt1(f))
      case Some(ko) => ko match {
        case Some(k) => fire(k)
        case None => discard
      }
    }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program, if any. */
  def thresholdOpt1[F[_], D, Δ](f: D => Option[Option[F[Unit]]]): (D, Δ) => Trigger[F, D, Δ] =
    new ((D, Δ) => Trigger[F, D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[F, D, Δ] = f(d) match {
        case None => sleep(this)
        case Some(ko) => ko match {
          case Some(k) => fire(k)
          case None => discard
        }
      }
    }

  def untilRight[F[_], D, Δ](f: D => Either[F[Unit], F[Unit]])(implicit F: Functor[F]): D => Trigger[F, D, Δ] =
    d => f(d) match {
      case Left(k) => reconsider(k.as(sleep(untilRight((d, δ) => f(d)))))
      case Right(k) => fire(k)
    }

  def untilRight[F[_], D, Δ](f: (D, Δ) => Either[F[Unit], F[Unit]])(implicit F: Functor[F]): (D, Δ) => Trigger[F, D, Δ] =
    new ((D, Δ) => Trigger[F, D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[F, D, Δ] = f(d, δ) match {
        case Left(k) => reconsider(k.as(sleep(this)))
        case Right(k) => fire(k)
      }
    }

  def continually[F[_], D, Δ](f: D => F[Unit])(implicit F: Functor[F]): D => Trigger[F, D, Δ] =
    d => reconsider(f(d).as(sleep(continually((d, δ) => f(d)))))

  def continually[F[_], D, Δ](f: (D, Δ) => F[Unit])(implicit F: Functor[F]): (D, Δ) => Trigger[F, D, Δ] =
    new ((D, Δ) => Trigger[F, D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[F, D, Δ] = reconsider(f(d, δ).as(sleep(this)))
    }
}

private[nutcracker] sealed trait SeqTrigger[Tok[_], K, D, Δ[_, _], D1] {

  import SeqTrigger._

  def map[L](f: K => L): SeqTrigger[Tok, L, D, Δ, D1] = this match {
    case Discard() => Discard()
    case Sleep(h) => Sleep(h.map(f))
    case Fire(k) => Fire(f(k))
    case Reconsider(cont) => Reconsider(cont andThen f)
  }
}

private[nutcracker] object SeqTrigger {

  case class Discard[Tok[_], K, D, Δ[_, _], D1]() extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Sleep[Tok[_], K, D, Δ[_, _], D1](h: SeqHandler[Tok, K, D, Δ, D1]) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Fire[Tok[_], K, D, Δ[_, _], D1](cont: K) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Reconsider[Tok[_], K, D, Δ[_, _], D1](cont: Tok[D1] => K) extends SeqTrigger[Tok, K, D, Δ, D1]

}

private[nutcracker] trait SeqHandler[Tok[_], K, D, Δ[_, _], D1] {
  self =>
  def handle[D2 <: D](d2: D2, δ: Δ[D1, D2]): SeqTrigger[Tok, K, D, Δ, D2]

  def map[L](f: K => L): SeqHandler[Tok, L, D, Δ, D1] = new SeqHandler[Tok, L, D, Δ, D1] {
    def handle[D2 <: D](d2: D2, δ: Δ[D1, D2]): SeqTrigger[Tok, L, D, Δ, D2] =
      self.handle(d2, δ).map(f)
  }
}

private[nutcracker] trait SeqPreHandler[Tok[_], K, D, Δ[_, _]] { self =>
  def handle[D0 <: D](d: D0): SeqTrigger[Tok, K, D, Δ, D0]

  def map[L](f: K => L): SeqPreHandler[Tok, L, D, Δ] = new SeqPreHandler[Tok, L, D, Δ] {
    def handle[D0 <: D](d: D0): SeqTrigger[Tok, L, D, Δ, D0] =
      self.handle(d).map(f)
  }
}