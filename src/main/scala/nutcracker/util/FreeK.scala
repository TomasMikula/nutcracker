package nutcracker.util

import scala.language.{higherKinds, implicitConversions}
import scalaz.{Applicative, BindRec, FreeT, Monad, ~>}
import scalaz.Id._

final case class FreeK[F[_[_], _], A](run: FreeT[F[FreeK[F, ?], ?], Id, A]) extends AnyVal {
  import FreeK._

  def flatMap[B](f: A => FreeK[F, B]): FreeK[F, B] = bind(this)(f)
  def >>=[B](f: A => FreeK[F, B]): FreeK[F, B] = flatMap(f)
  def >>[B](fb: => FreeK[F, B])(implicit AIsUnit: A =:= Unit): FreeK[F, B] = this >>= { _ => fb }
  def >>>=[G[_[_], _], B](f: A => FreeK[G, B])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): FreeK[G, B] = injectTransform(inj, FK)(this) flatMap f
  def >>>[G[_[_], _], B](gb: => FreeK[G, B])(implicit
    AIsUnit: A =:= Unit,
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): FreeK[G, B] = this >>>= { _ => gb }
  def map[B](f: A => B): FreeK[F, B] = FreeK(run.map(f))

  def inj[G[_[_], _]](implicit tr: (FreeK[F, ?] ~> FreeK[G, ?])): FreeK[G, A] = tr(this)

  // sometimes it is easier for scalac to find these implicit arguments than for inj above
  def inject[G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]): FreeK[G, A] = FreeK.injectTransform(inj, FK)(this)

  type K[T] = FreeK[F, T]

  final def foldMap[M[_]](tr: F[K, ?] ~> M)(implicit M0: BindRec[M], M1: Applicative[M]): M[A] =
    run.hoist(idToM[M]).foldMap(tr)
}

object FreeK {

  def pure[F[_[_], _], A](a: A): FreeK[F, A] =
    FreeK(FreeT.point(a))

  def suspend[F[_[_], _], A](a: F[FreeK[F, ?], A]): FreeK[F, A] =
    FreeK(FreeT.liftF[F[FreeK[F, ?], ?], Id, A](a))

  def bind[F[_[_], _], A1, A2](fa: FreeK[F, A1])(f: A1 => FreeK[F, A2]): FreeK[F, A2] =
    FreeK(fa.run.flatMap(f(_).run))

  def lift[F[_[_], _], G[_[_], _], A](a: F[FreeK[G, ?], A])(implicit inj: InjectK[F, G]): FreeK[G, A] =
    suspend(inj(a))

  implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = new Monad[FreeK[F, ?]] {
    def point[A](a: => A): FreeK[F, A] = FreeK.pure(a)
    def bind[A, B](fa: FreeK[F, A])(f: A => FreeK[F, B]): FreeK[F, B] = FreeK.bind(fa)(f)
  }

  implicit def injectTransform[F[_[_], _], G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): (FreeK[F, ?] ~> FreeK[G, ?]) =
    new (FreeK[F, ?] ~> FreeK[G, ?]) { self =>

      val tr = new (F[FreeK[F, ?], ?] ~> G[FreeK[G, ?], ?]) {
        def apply[A](fa: F[FreeK[F, ?], A]): G[FreeK[G, ?], A] =
          inj(FK.transform(fa)(self))
      }

      def apply[A](fa: FreeK[F, A]): FreeK[G, A] = FreeK(fa.run.interpret(tr))
    }

  implicit def autoInject[F[_[_], _], G[_[_], _], A](f: FreeK[F, A])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): FreeK[G, A] = injectTransform(inj, FK)(f)
}
