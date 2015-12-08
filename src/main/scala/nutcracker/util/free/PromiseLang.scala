package nutcracker.util.free

import scala.language.higherKinds
import scalaz.~>

/**
  *
  * @tparam K type of continuation that can be registered to execute
  *           when promise is completed
  * @tparam A type of promised value
  */
sealed trait PromiseLang[K[_], A]

object PromiseLang {

  final case class Promised[A](id: Long)

  case class Promise[K[_], A]() extends PromiseLang[K, Promised[A]]
  case class Complete[K[_], A](p: Promised[A], a: A) extends PromiseLang[K, Unit]
  case class OnComplete[K[_], A](p: Promised[A], f: A => K[Unit]) extends PromiseLang[K, Unit]

  def promise[K[_], A]: PromiseLang[K, Promised[A]] = Promise[K, A]()
  def complete[K[_], A](p: Promised[A], a: A): PromiseLang[K, Unit] = Complete[K, A](p, a)
  def onComplete[K[_], A](p: Promised[A])(f: A => K[Unit]): PromiseLang[K, Unit] = OnComplete(p, f)

  def promiseF[A]: FreeK[PromiseLang, Promised[A]] = FreeK.lift(promise[FreeK[PromiseLang, ?], A])
  def completeF[A](p: Promised[A], a: A): FreeK[PromiseLang, Unit] = FreeK.lift(complete[FreeK[PromiseLang, ?], A](p, a))


  implicit def functorKInstance: FunctorKA[PromiseLang] = new FunctorKA[PromiseLang] {

    def transform[K[_], L[_], A](pk: PromiseLang[K, A])(tr: K ~> L): PromiseLang[L, A] = pk match {
      case Promise() => Promise()
      case Complete(p, a) => Complete(p, a)
      case OnComplete(p, f) => onComplete(p){ a => tr(f(a)) }
    }
  }
}