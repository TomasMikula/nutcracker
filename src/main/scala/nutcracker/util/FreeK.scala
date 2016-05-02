package nutcracker.util

import scala.language.{higherKinds, implicitConversions}
import scalaz._

sealed trait FreeK[F[_[_], _], A] {
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
  def map[B](f: A => B): FreeK[F, B] = flatMap { a => Pure(f(a)) }

  def inj[G[_[_], _]](implicit tr: (FreeK[F, ?] ~> FreeK[G, ?])): FreeK[G, A] = tr(this)

  // sometimes it is easier for scalac to find these implicit arguments than for inj above
  def inject[G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]): FreeK[G, A] = FreeK.injectTransform(inj, FK)(this)

  type K[T] = FreeK[F, T]

  final def foldMap[M[_]](tr: F[K, ?] ~> M)(implicit M: Monad[M]): M[A] =
    this match {
      case Pure(a) => M.point(a)
      case Suspend(fa) => tr(fa)
      case Bind(fx, f) => M.bind(tr(fx))(f(_).foldMap(tr))
    }
}

object FreeK {
  case class Pure[F[_[_], _], A](a: A) extends FreeK[F, A]
  case class Suspend[F[_[_], _], A](a: F[FreeK[F, ?], A]) extends FreeK[F, A]
  case class Bind[F[_[_], _], A1, A2](a: F[FreeK[F, ?], A1], f: A1 => FreeK[F, A2]) extends FreeK[F, A2]

  def pure[F[_[_], _], A](a: A): FreeK[F, A] = Pure(a)
  def suspend[F[_[_], _], A](a: F[FreeK[F, ?], A]): FreeK[F, A] = Suspend(a)
  def bind[F[_[_], _], A1, A2](a: FreeK[F, A1])(f: A1 => FreeK[F, A2]): FreeK[F, A2] = a match {
    case Pure(a1) => f(a1)
    case Suspend(ffa1) => Bind(ffa1, f)
    case Bind(ffa0, g) => bind0(ffa0)({ g(_) >>= f })
  }
  private def bind0[F[_[_], _], A1, A2](a: F[FreeK[F, ?], A1])(f: A1 => FreeK[F, A2]): FreeK[F, A2] = Bind(a, f)

  def lift[F[_[_], _], G[_[_], _], A](a: F[FreeK[G, ?], A])(implicit inj: InjectK[F, G]): FreeK[G, A] =
    suspend(inj.inj[FreeK[G, ?], A](a))

  implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = new Monad[FreeK[F, ?]] {
    def point[A](a: => A): FreeK[F, A] = Pure(a)
    def bind[A, B](fa: FreeK[F, A])(f: A => FreeK[F, B]): FreeK[F, B] = FreeK.bind(fa)(f)
  }

  implicit def injectTransform[F[_[_], _], G[_[_], _]](implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): (FreeK[F, ?] ~> FreeK[G, ?]) =
    new ~>[FreeK[F, ?], FreeK[G, ?]] {
      def apply[A](fa: FreeK[F, A]): FreeK[G, A] = fa match {
        case Pure(a) => Pure(a)
        case Suspend(a) => Suspend(inj.inj[FreeK[G, ?], A](FK.transform[FreeK[F, ?], FreeK[G, ?], A](a)(this)))
        case Bind(a, f) => apply(suspend(a)) flatMap { aa => apply(f(aa)) }
      }
    }

  implicit def autoInject[F[_[_], _], G[_[_], _], A](f: FreeK[F, A])(implicit
    inj: InjectK[F, G],
    FK: FunctorKA[F]
  ): FreeK[G, A] = injectTransform(inj, FK)(f)
}
