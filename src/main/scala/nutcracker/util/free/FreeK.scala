package nutcracker.util.free

import scala.annotation.tailrec
import scala.language.{higherKinds, implicitConversions}
import scalaz._

sealed trait FreeK[F[_[_], _], A] {
  import FreeK._

  def flatMap[A2](f: A => FreeK[F, A2]): FreeK[F, A2] = Bind(this, f)
  def >>=[A2](f: A => FreeK[F, A2]): FreeK[F, A2] = flatMap(f)

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

  implicit def freeKMonad[F[_[_], _]]: Monad[FreeK[F, ?]] = new Monad[FreeK[F, ?]] {
    def point[A](a: => A): FreeK[F, A] = Pure(a)
    def bind[A, B](fa: FreeK[F, A])(f: (A) => FreeK[F, B]): FreeK[F, B] = Bind(fa, f)
  }
}
