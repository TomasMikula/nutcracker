package nutcracker.util.free

import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions}
import scalaz.Leibniz.{===, refl}
import scalaz._

sealed trait FreeK[F[_[_], _], A] {
  import FreeK._

  def flatMap[B](f: A => FreeK[F, B]): FreeK[F, B] = Bind(this, f)
  def >>=[B](f: A => FreeK[F, B]): FreeK[F, B] = flatMap(f)
  def >>>=[G[_[_], _], B](f: A => FreeK[G, B])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): FreeK[G, B] = injectTransform(inj, FK)(this) flatMap f
  def map[B](f: A => B): FreeK[F, B] = flatMap { a => Pure(f(a)) }

  def inj[G[_[_], _]](implicit tr: (FreeK[F, ?] ~> FreeK[G, ?])): FreeK[G, A] = tr(this)

  // sometimes it is easier for scalac to find these implicit arguments than for inj above
  def inject[G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]): FreeK[G, A] = FreeK.injectTransform(inj, FK)(this)

  type K[T] = FreeK[F, T]

  @tailrec final def resume(implicit F: Functor[F[K, ?]]): (F[K, FreeK[F, A]] \/ A) =
    this match {
      case Pure(a) => \/-(a)
      case Suspend(fa) => -\/(F.map(fa)(Pure(_)))
      case Bind(x, f) => x match {
        case Pure(a) => f(a).resume
        case Suspend(fx) => -\/(F.map(fx)(f))
        case Bind(y, g) => (y flatMap { g(_).flatMap(f) }).resume
      }
    }
}

object FreeK {
  case class Pure[F[_[_], _], A](a: A) extends FreeK[F, A]
  case class Suspend[F[_[_], _], A](a: F[FreeK[F, ?], A]) extends FreeK[F, A]
  case class Bind[F[_[_], _], A1, A2](a: FreeK[F, A1], f: A1 => FreeK[F, A2]) extends FreeK[F, A2]

  def lift[F[_[_], _], A](a: F[FreeK[F, ?], A]): FreeK[F, A] = Suspend(a)

  implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = new Monad[FreeK[F, ?]] {
    def point[A](a: => A): FreeK[F, A] = Pure(a)
    def bind[A, B](fa: FreeK[F, A])(f: (A) => FreeK[F, B]): FreeK[F, B] = Bind(fa, f)
  }

  implicit class FreeKUnitOps[F[_[_], _]](fu: FreeK[F, Unit]) {
    def >>[B](fb: FreeK[F, B]): FreeK[F, B] = fu >>= { _ => fb }
    def >>>[G[_[_], _], B](gb: FreeK[G, B])(implicit
      inj: InjectK[F, G],
      FK: FunctorKA[F]
    ): FreeK[G, B] = fu >>>= { _ => gb }
  }

  implicit def injectTransform[F[_[_], _], G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): (FreeK[F, ?] ~> FreeK[G, ?]) =
    new ~>[FreeK[F, ?], FreeK[G, ?]] {
      def apply[A](fa: FreeK[F, A]): FreeK[G, A] = fa match {
        case Pure(a) => Pure(a)
        case Suspend(a) => Suspend(inj.inj[FreeK[G, ?], A](FK.transform[FreeK[F, ?], FreeK[G, ?], A](a)(this)))
        case Bind(a, f) => apply(a) flatMap { aa => apply(f(aa)) }
      }
    }

  implicit def autoInject[F[_[_], _], G[_[_], _], A](f: FreeK[F, A])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): FreeK[G, A] = injectTransform(inj, FK)(f)
}
