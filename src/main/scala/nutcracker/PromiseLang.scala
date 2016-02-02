package nutcracker

import nutcracker.util.free.{FreeK, FunctorKA, InjectK}

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Apply, Cont, ~>}

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

  def promiseC[F[_[_], _], A](cont: Cont[FreeK[F, Unit], A])(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[A]] = for {
    pa <- promiseF[A].inject[F]
    _ <- cont(completeF(pa, _))
  } yield pa

  // Scalac doesn't seem to always pick up the Applicative instance and syntax for Cont[FreeK[F, Unit], ?],
  // so we provide this API for convenience.
  def promiseC[F[_[_], _]]: PromiseContBuilder[F] = PromiseContBuilder()

  case class PromiseContBuilder[F[_[_], _]]() {
    private type Kont[A] = Cont[FreeK[F, Unit], A]
    private val A = Apply[Kont]

    def tuple[A1, A2](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2])(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[(A1, A2)]] =
      promiseC(A.tuple2(a1, a2))
    def tuple[A1, A2, A3](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3])(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[(A1, A2, A3)]] =
      promiseC(A.tuple3(a1, a2, a3))
    def tuple[A1, A2, A3, A4](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4])(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[(A1, A2, A3, A4)]] =
      promiseC(A.tuple4(a1, a2, a3, a4))
    def tuple[A1, A2, A3, A4, A5](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4], a5: Cont[FreeK[F, Unit], A5])(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[(A1, A2, A3, A4, A5)]] =
      promiseC(A.tuple5(a1, a2, a3, a4, a5))

    def apply[A1, A2, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2])(f: (A1, A2) => R)(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[R]] =
      promiseC(A.apply2(a1, a2)(f))
    def apply[A1, A2, A3, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3])(f: (A1, A2, A3) => R)(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[R]] =
      promiseC(A.apply3(a1, a2, a3)(f))
    def apply[A1, A2, A3, A4, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4])(f: (A1, A2, A3, A4) => R)(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[R]] =
      promiseC(A.apply4(a1, a2, a3, a4)(f))
    def apply[A1, A2, A3, A4, A5, R](a1: Cont[FreeK[F, Unit], A1], a2: Cont[FreeK[F, Unit], A2], a3: Cont[FreeK[F, Unit], A3], a4: Cont[FreeK[F, Unit], A4], a5: Cont[FreeK[F, Unit], A5])(f: (A1, A2, A3, A4, A5) => R)(implicit inj: InjectK[PromiseLang, F]): FreeK[F, Promised[R]] =
      promiseC(A.apply5(a1, a2, a3, a4, a5)(f))
  }


  implicit def functorKInstance: FunctorKA[PromiseLang] = new FunctorKA[PromiseLang] {

    def transform[K[_], L[_], A](pk: PromiseLang[K, A])(tr: K ~> L): PromiseLang[L, A] = pk match {
      case Promise() => Promise()
      case Complete(p, a) => Complete(p, a)
      case OnComplete(p, f) => onComplete(p){ a => tr(f(a)) }
    }
  }
}