package nutcracker.util

import nutcracker.util.free.Free
import scalaz.Monad

/** Free monad for type constructors of kind `F[_[_], _]`,
  * where `F`'s first type parameter is recursively set to FreeK[F, ?].
  * If we pretend that recursive type aliases are legal, then `FreeK` is
  * equivalent to
  *
  * {{{
  * type FreeK[F[_[_], _], A] = Free[F[FreeK[F, ?], ?], A]
  * }}}
  *
  * This is useful for instruction sets (a.k.a. algebras, DSLs, ...) that
  * need to refer to the type of the free program that they are embedded in.
  */
final class FreeK[F[_[_], _], A](val unwrap: Free[F[FreeK[F, ?], ?], A]) { // extends AnyVal { // https://issues.scala-lang.org/browse/SI-7685
  import FreeK._

  def map[B](f: A => B): FreeK[F, B] =
    wrap(unwrap.map(f))

  def flatMap[B](f: A => FreeK[F, B]): FreeK[F, B] =
    wrap(unwrap.flatMap(f(_).unwrap))

  def >>=[B](f: A => FreeK[F, B]): FreeK[F, B] =
    flatMap(f)

  def >>[B](fb: => FreeK[F, B])(implicit AIsUnit: A =:= Unit): FreeK[F, B] =
    this >>= { _ => fb }
}

object FreeK {

  @inline
  private def wrap[F[_[_], _], A](fa: Free[F[FreeK[F, ?], ?], A]): FreeK[F, A] =
    new FreeK(fa)

  def pure[F[_[_], _], A](a: A): FreeK[F, A] =
    wrap(Free.point(a))

  def liftF[F[_[_], _], A](a: F[FreeK[F, ?], A]): FreeK[F, A] =
    wrap(Free.liftF[F[FreeK[F, ?], ?], A](a))

  implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] =
    new Monad[FreeK[F, ?]] {
      def point[A](a: => A): FreeK[F, A] = FreeK.pure(a)
      def bind[A, B](fa: FreeK[F, A])(f: A => FreeK[F, B]): FreeK[F, B] = fa.flatMap(f)
    }
}