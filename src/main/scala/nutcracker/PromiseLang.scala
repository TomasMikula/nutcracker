package nutcracker

import nutcracker.util.free.{FreeK, FunctorKA, InjectK}

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Cont, ~>}

/**
  *
  * @tparam K type of continuation that can be registered to execute
  *           when promise is completed
  * @tparam A type of promised value
  */
sealed trait PromiseLang[K[_], A]

object PromiseLang {

  final case class Promised[A](id: Long) { self =>

    def asCont[F[_[_], _]](implicit inj: InjectK[PromiseLang, F]): Cont[FreeK[F, Unit], A] =
      Cont { onCompleteF(self)(_) }

  }

  object Promised {

    implicit def toCont[F[_[_], _], A](p: Promised[A])(implicit inj: InjectK[PromiseLang, F]): Cont[FreeK[F, Unit], A] =
      p.asCont

  }

  case class Promise[K[_], A]() extends PromiseLang[K, Promised[A]]
  case class Complete[K[_], A](p: Promised[A], a: A) extends PromiseLang[K, Unit]
  case class OnComplete[K[_], A](p: Promised[A], f: A => K[Unit]) extends PromiseLang[K, Unit]

  def promise[K[_], A]: PromiseLang[K, Promised[A]] = Promise[K, A]()
  def complete[K[_], A](p: Promised[A], a: A): PromiseLang[K, Unit] = Complete[K, A](p, a)
  def onComplete[K[_], A](p: Promised[A])(f: A => K[Unit]): PromiseLang[K, Unit] = OnComplete(p, f)

  def promiseF[A]: FreeK[PromiseLang, Promised[A]] = FreeK.lift(promise[FreeK[PromiseLang, ?], A])
  def completeF[A](p: Promised[A], a: A): FreeK[PromiseLang, Unit] = FreeK.lift(complete[FreeK[PromiseLang, ?], A](p, a))
  def onCompleteF[F[_[_], _], A](p: Promised[A])(f: A => FreeK[F, Unit])(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Unit] =
    FreeK.lift(onComplete[FreeK[F, ?], A](p)(f))


  implicit def functorKInstance: FunctorKA[PromiseLang] = new FunctorKA[PromiseLang] {

    def transform[K[_], L[_], A](pk: PromiseLang[K, A])(tr: K ~> L): PromiseLang[L, A] = pk match {
      case Promise() => Promise()
      case Complete(p, a) => Complete(p, a)
      case OnComplete(p, f) => onComplete(p){ a => tr(f(a)) }
    }
  }
}