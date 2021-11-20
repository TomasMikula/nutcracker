package nutcracker.util

import nutcracker.util.free.FreeT
import scalaz.{Applicative, BindRec, Monad, ~>}

final case class FreeKT[F[_[_], _], M[_], A](run: FreeT[F[FreeKT[F, M, *], *], M, A]) { // extends AnyVal { // https://issues.scala-lang.org/browse/SI-7685

  def map[B](f: A => B)(implicit M: Applicative[M]): FreeKT[F, M, B] =
    FreeKT(run.map(f))

  def flatMap[B](f: A => FreeKT[F, M, B]): FreeKT[F, M, B] =
    FreeKT(run.flatMap(f(_).run))

  def >>=[B](f: A => FreeKT[F, M, B]): FreeKT[F, M, B] =
    flatMap(f)

  def >>[B](fb: => FreeKT[F, M, B])(implicit AIsUnit: A =:= Unit): FreeKT[F, M, B] =
    this >>= { _ => fb }

  def effect(f: A => FreeKT[F, M, Unit])(implicit M: Applicative[M]): FreeKT[F, M, A] = for {
    a <- this
    _ <- f(a)
  } yield a

  def >>>=[G[_[_], _], B](f: A => FreeKT[G, M, B])(implicit
    inj: Inject[F[FreeKT[G, M, *], *], G[FreeKT[G, M, *], *]],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, B] =
    inject[G] flatMap f

  def >>>[G[_[_], _], B](gb: => FreeKT[G, M, B])(implicit
    AIsUnit: A =:= Unit,
    inj: Inject[F[FreeKT[G, M, *], *], G[FreeKT[G, M, *], *]],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, B] =
    this >>>= { _ => gb }

  def inject[G[_[_], _]](implicit
    inj: Inject[F[FreeKT[G, M, *], *], G[FreeKT[G, M, *], *]],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, A] =
    FreeKT.injection[F, G, M].apply(this)

  def hoist[N[_]](mn: M ~> N)(implicit FK: FunctorKA[F]): FreeKT[F, N, A] =
    FreeKT.hoist[F, M, N](mn).apply(this)

  def foldMap(tr: F[FreeKT[F, M, *], *] ~> M)(implicit M: BindRec[M]): M[A] =
    run.foldMap(tr)
}

object FreeKT {

  def pure[F[_[_], _], M[_], A](a: A)(implicit M: Applicative[M]): FreeKT[F, M, A] =
    FreeKT(FreeT.point(a))

  def liftF[F[_[_], _], M[_], A](a: F[FreeKT[F, M, *], A]): FreeKT[F, M, A] =
    FreeKT(FreeT.liftF[F[FreeKT[F, M, *], *], M, A](a))

  implicit def freeKTMonad[F[_[_], _], M[_]](implicit M: Applicative[M]): Monad[FreeKT[F, M, *]] =
    new Monad[FreeKT[F, M, *]] {
      def point[A](a: => A): FreeKT[F, M, A] = FreeKT.pure(a)
      def bind[A, B](fa: FreeKT[F, M, A])(f: A => FreeKT[F, M, B]): FreeKT[F, M, B] = fa.flatMap(f)
    }

  def injection[F[_[_], _], G[_[_], _], M[_]](implicit
    inj: Inject[F[FreeKT[G, M, *], *], G[FreeKT[G, M, *], *]],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[F, M, *] ~> FreeKT[G, M, *] =
    new (FreeKT[F, M, *] ~> FreeKT[G, M, *]) { self =>
      val tr = λ[F[FreeKT[F, M, *], *] ~> G[FreeKT[G, M, *], *]] {
        fa => inj(FK.transform(fa)(self))
      }

      def apply[A](fa: FreeKT[F, M, A]): FreeKT[G, M, A] =
        FreeKT(fa.run.interpret(tr))
    }

  def hoist[F[_[_], _], M[_], N[_]](mn: M ~> N)(implicit FK: FunctorKA[F]): (FreeKT[F, M, *] ~> FreeKT[F, N, *]) =
    new (FreeKT[F, M, *] ~> FreeKT[F, N, *]) { self =>

      val tr = new (F[FreeKT[F, M, *], *] ~> F[FreeKT[F, N, *], *]) {
        def apply[A](fa: F[FreeKT[F, M, *], A]): F[FreeKT[F, N, *], A] =
          FK.transform(fa)(self)
      }

      def apply[A](fa: FreeKT[F, M, A]): FreeKT[F, N, A] =
        FreeKT(fa.run.hoist(mn).interpret(tr))
    }
}
