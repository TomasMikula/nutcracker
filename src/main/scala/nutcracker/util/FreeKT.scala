package nutcracker.util

import scala.language.{higherKinds, implicitConversions}
import scalaz.{Applicative, BindRec, FreeT, Functor, Monad, MonadPartialOrder, ~>}

final case class FreeKT[F[_[_], _], M[_], A](run: FreeT[F[FreeKT[F, M, ?], ?], M, A]) extends AnyVal {

  def map[B](f: A => B)(implicit M: Applicative[M]): FreeKT[F, M, B] =
    FreeKT(run.map(f))

  def flatMap[B](f: A => FreeKT[F, M, B]): FreeKT[F, M, B] =
    FreeKT(run.flatMap(f(_).run))

  def >>=[B](f: A => FreeKT[F, M, B]): FreeKT[F, M, B] =
    flatMap(f)

  def >>[B](fb: => FreeKT[F, M, B])(implicit AIsUnit: A =:= Unit): FreeKT[F, M, B] =
    this >>= { _ => fb }

  def >>>=[G[_[_], _], B](f: A => FreeKT[G, M, B])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, B] =
    inject[G] flatMap f

  def >>>[G[_[_], _], B](gb: => FreeKT[G, M, B])(implicit
    AIsUnit: A =:= Unit,
    inj: InjectK[F, G],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, B] =
    this >>>= { _ => gb }

  def inj[G[_[_], _]](implicit tr: (FreeKT[F, M, ?] ~> FreeKT[G, M, ?])): FreeKT[G, M, A] =
    tr(this)

  // sometimes it is easier for scalac to find these implicit arguments than for inj above
  def inject[G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, A] =
    FreeKT.injectionOrder[F, G, M].apply(this)

  def hoist[N[_]](mn: M ~> N)(implicit FK: FunctorKA[F], N: Functor[N]): FreeKT[F, N, A] =
    FreeKT.hoist[F, M, N](mn).apply(this)

  def promote[N[_]](implicit mn: MonadPartialOrder[N, M], FK: FunctorKA[F]): FreeKT[F, N, A] =
    hoist(mn)(FK, mn.MG)

  def foldMap(tr: F[FreeKT[F, M, ?], ?] ~> M)(implicit M0: BindRec[M], M1: Applicative[M]): M[A] =
    run.foldMap(tr)

  def foldMapN[N[_]](tr: F[FreeKT[F, M, ?], ?] ~> N)(implicit
    mn: MonadPartialOrder[N, M],
    N0: BindRec[N],
    N1: Applicative[N]
  ): N[A] =
    run.hoist(mn).foldMap(tr)
}

object FreeKT {

  def pure[F[_[_], _], M[_], A](a: A)(implicit M: Applicative[M]): FreeKT[F, M, A] =
    FreeKT(FreeT.point(a))

  def liftF[F[_[_], _], M[_], A](a: F[FreeKT[F, M, ?], A])(implicit M: Applicative[M]): FreeKT[F, M, A] =
    FreeKT(FreeT.liftF[F[FreeKT[F, M, ?], ?], M, A](a))

  def injLiftF[F[_[_], _], G[_[_], _], M[_], A](
    a: F[FreeKT[G, M, ?], A])(implicit
    M: Applicative[M],
    inj: InjectK[F, G]
  ): FreeKT[G, M, A] =
    liftF(inj(a))

  implicit def freeKTMonad[F[_[_], _], M[_]](implicit M: Applicative[M]): Monad[FreeKT[F, M, ?]] =
    new Monad[FreeKT[F, M, ?]] {
      def point[A](a: => A): FreeKT[F, M, A] = FreeKT.pure(a)
      def bind[A, B](fa: FreeKT[F, M, A])(f: A => FreeKT[F, M, B]): FreeKT[F, M, B] = fa.flatMap(f)
    }

  implicit def injectionOrder[F[_[_], _], G[_[_], _], M[_]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): MonadPartialOrder[FreeKT[G, M, ?], FreeKT[F, M, ?]] =
    new MonadPartialOrder[FreeKT[G, M, ?], FreeKT[F, M, ?]] { self =>
      override val MG = freeKTMonad[G, M]
      override val MF = freeKTMonad[F, M]

      val tr = new (F[FreeKT[F, M, ?], ?] ~> G[FreeKT[G, M, ?], ?]) {
        def apply[A](fa: F[FreeKT[F, M, ?], A]): G[FreeKT[G, M, ?], A] =
          inj(FK.transform(fa)(self))
      }

      def promote[A](fa: FreeKT[F, M, A]): FreeKT[G, M, A] =
        FreeKT(fa.run.interpret(tr))
    }

  def hoist[F[_[_], _], M[_], N[_]](mn: M ~> N)(implicit
    FK: FunctorKA[F],
    N: Functor[N]
  ): (FreeKT[F, M, ?] ~> FreeKT[F, N, ?]) =
    new (FreeKT[F, M, ?] ~> FreeKT[F, N, ?]) { self =>

      val tr = new (F[FreeKT[F, M, ?], ?] ~> F[FreeKT[F, N, ?], ?]) {
        def apply[A](fa: F[FreeKT[F, M, ?], A]): F[FreeKT[F, N, ?], A] =
          FK.transform(fa)(self)
      }

      def apply[A](fa: FreeKT[F, M, A]): FreeKT[F, N, A] =
        FreeKT(fa.run.hoist(mn).interpret(tr))
    }

  implicit def autoInject[F[_[_], _], G[_[_], _], M[_], A](f: FreeKT[F, M, A])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F],
    M: Applicative[M]
  ): FreeKT[G, M, A] = f.inject[G]
}