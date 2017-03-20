package nutcracker

import scala.language.higherKinds
import scalaz.{Functor, ~>}
import scalaz.Id._
import scalaz.syntax.functor._

sealed trait TriggerF[F[_], A]

object TriggerF {

  case class Discard[F[_], A]() extends TriggerF[F, A]
  case class Fire[F[_], A](exec: F[Unit]) extends TriggerF[F, A]
  case class Sleep[F[_], A](next: A) extends TriggerF[F, A]
  case class FireReload[F[_], A](cont: F[A]) extends TriggerF[F, A]
}

final case class Trigger[F[_], D, Δ](unfix: TriggerF[F, (D, Δ) => Trigger[F, D, Δ]]) { // extends AnyVal { // https://issues.scala-lang.org/browse/SI-9600
  import Trigger._
  import TriggerF._

  def contramap[D0, Δ0](f: D0 => D, g: Δ0 => Δ)(implicit F: Functor[F]): Trigger[F, D0, Δ0] = unfix match {
    case Discard() => discard
    case Sleep(next) => sleep((d0, δ0) => next(f(d0), g(δ0)).contramap(f, g))
    case Fire(exec) => fire(exec)
    case FireReload(cont) => fireReload(cont map (next => (d0, δ0) => next(f(d0), g(δ0)).contramap(f, g)))
  }
}

object Trigger {
  import TriggerF._

  def discard[F[_], D, Δ]: Trigger[F, D, Δ] = Trigger(Discard())
  def sleep[F[_], D, Δ](next: (D, Δ) => Trigger[F, D, Δ]): Trigger[F, D, Δ] = Trigger(Sleep(next))
  def fire[F[_], D, Δ](exec: F[Unit]): Trigger[F, D, Δ] = Trigger(Fire(exec))
  def fireReload[F[_], D, Δ](cont: F[(D, Δ) => Trigger[F, D, Δ]]): Trigger[F, D, Δ] = Trigger(FireReload(cont))

  def observer[F[_], D, Δ](f: Id ~> λ[α => (D, Δ) => TriggerF[F, α]]): (D, Δ) => Trigger[F, D, Δ] =
    new ((D, Δ) => Trigger[F, D, Δ]) {
      val g: (D, Δ) => TriggerF[F, (D, Δ) => Trigger[F, D, Δ]] = f[(D, Δ) => Trigger[F, D, Δ]](this)

      def apply(d: D, δ: Δ): Trigger[F, D, Δ] = Trigger(g(d, δ))
    }

  def valueObserver[F[_], D, Δ](f: Id ~> λ[α => D => TriggerF[F, α]]): D => Trigger[F, D, Δ] =
    new (D => Trigger[F, D, Δ]) {
      val g: D => TriggerF[F, (D, Δ) => Trigger[F, D, Δ]] = f[(D, Δ) => Trigger[F, D, Δ]]((d, δ) => this(d))

      def apply(d: D): Trigger[F, D, Δ] = Trigger(g(d))
    }

  /** Keep trying `f` until it returns `Some`. */
  def threshold[F[_], D, Δ](f: D => Option[F[Unit]]): D => Trigger[F, D, Δ] =
    valueObserver[F, D, Δ](λ[Id ~> λ[α => D => TriggerF[F, α]]](α => d => f(d) match {
      case None => Sleep(α)
      case Some(k) => Fire(k)
    }))

  /** Keep trying `f` until it returns `Some`. */
  def threshold1[F[_], D, Δ](f: D => Option[F[Unit]]): (D, Δ) => Trigger[F, D, Δ] =
    observer[F, D, Δ](λ[Id ~> λ[α => (D, Δ) => TriggerF[F, α]]](α => (d, _) => f(d) match {
      case None => Sleep(α)
      case Some(k) => Fire(k)
    }))

  def untilRight[F[_], D, Δ](f: D => Either[F[Unit], F[Unit]])(implicit F: Functor[F]): D => Trigger[F, D, Δ] =
    valueObserver[F, D, Δ](λ[Id ~> λ[α => D => TriggerF[F, α]]](α => d => f(d) match {
      case Left(k) => FireReload(k map (_ => α))
      case Right(k) => Fire(k)
    }))

  def continually[F[_], D, Δ](f: D => F[Unit])(implicit F: Functor[F]): D => Trigger[F, D, Δ] =
    valueObserver[F, D, Δ](λ[Id ~> λ[α => D => TriggerF[F, α]]](α => d => FireReload(f(d) map (_ => α))))

  def continually[F[_], D, Δ](f: (D, Δ) => F[Unit])(implicit F: Functor[F]): (D, Δ) => Trigger[F, D, Δ] =
    observer[F, D, Δ](λ[Id ~> λ[α => (D, Δ) => TriggerF[F, α]]](α => (d, δ) => FireReload(f(d, δ) map (_ => α))))
}

private[nutcracker] sealed trait SeqTrigger[Tok[_], K, D, Δ[_, _], D1] {

  import SeqTrigger._

  def map[L](f: K => L): SeqTrigger[Tok, L, D, Δ, D1] = this match {
    case Discard() => Discard()
    case Sleep(h) => Sleep(h.map(f))
    case Fire(k) => Fire(f(k))
    case FireReload(cont) => FireReload(cont andThen f)
  }
}

private[nutcracker] object SeqTrigger {

  case class Discard[Tok[_], K, D, Δ[_, _], D1]() extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Sleep[Tok[_], K, D, Δ[_, _], D1](h: SeqHandler[Tok, K, D, Δ, D1]) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class Fire[Tok[_], K, D, Δ[_, _], D1](cont: K) extends SeqTrigger[Tok, K, D, Δ, D1]

  case class FireReload[Tok[_], K, D, Δ[_, _], D1](cont: Tok[D1] => K) extends SeqTrigger[Tok, K, D, Δ, D1]

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