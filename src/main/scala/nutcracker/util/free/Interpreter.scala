package nutcracker.util.free

import scala.language.higherKinds

import scala.annotation.tailrec
import scalaz._
import scalaz.syntax.monoid._

trait Interpreter[F[_[_], _], S[_[_]], W[_[_]]] {

  def step[K[_]: Applicative, A](f: F[K, A])(s: S[K]): (S[K], W[K], K[A])
  def uncons[K[_]: Applicative](w: W[K])(s: S[K]): Option[(K[Unit], W[K], S[K])]

  def runFree[A](
    s: S[FreeK[F, ?]],
    p: FreeK[F, A])(implicit
    W: Monoid[W[FreeK[F, ?]]]
  ): (S[FreeK[F, ?]], A) = runFree(s, p, W.zero)

  def runFreeUnit(
    s: S[FreeK[F, ?]],
    p: FreeK[F, Unit])(implicit
    W: Monoid[W[FreeK[F, ?]]]
  ): S[FreeK[F, ?]] = runFreeUnit(s, p, W.zero)

  @tailrec private def runFree[A](
    s: S[FreeK[F, ?]],
    p: FreeK[F, A],
    w: W[FreeK[F, ?]])(implicit
    W: Monoid[W[FreeK[F, ?]]]
  ): (S[FreeK[F, ?]], A) = p match {
    case FreeK.Pure(a) => (runFreeUnit(s, FreeK.Pure(()), w), a)
    case FreeK.Suspend(ffa) => step[FreeK[F, ?], A](ffa)(s) match {
      case (s1, w1, fa) => runFree(s1, fa, w |+| w1)
    }
    case bnd: FreeK.Bind[F, a1, A] => step[FreeK[F, ?], a1](bnd.a)(s) match {
      case (s1, w1, fx) => runFree(s1, fx >>= { bnd.f(_) }, w |+| w1)
    }
  }

  @tailrec private def runFreeUnit(
    s: S[FreeK[F, ?]],
    p: FreeK[F, Unit],
    w: W[FreeK[F, ?]])(implicit
    W: Monoid[W[FreeK[F, ?]]]
  ): S[FreeK[F, ?]] = p match {
    case FreeK.Pure(()) => uncons[FreeK[F, ?]](w)(s) match {
      case Some((cont, w1, s1)) => runFreeUnit(s1, cont, w1)
      case None => s
    }
    case FreeK.Suspend(ffu) => step[FreeK[F, ?], Unit](ffu)(s) match {
      case (s1, w1, fu) => runFreeUnit(s1, fu, w |+| w1)
    }
    case bnd: FreeK.Bind[F, a1, Unit] => step[FreeK[F, ?], a1](bnd.a)(s) match {
      case (s1, w1, fx) => runFreeUnit(s1, fx >>= { bnd.f(_) }, w |+| w1)
    }
  }
}

object Interpreter {

  type ConstK[T, K[_]] = T
  implicit def constMonoidK[T: Monoid]: MonoidK[ConstK[T, ?[_]]] = new MonoidK[ConstK[T, ?[_]]] {
    def zero[K[_]]: ConstK[T, K] = Monoid[T].zero
    def append[K[_]](f1: ConstK[T, K], f2: ConstK[T, K]): ConstK[T, K] = Monoid[T].append(f1, f2)
  }

  type AlwaysClean[K[_]] = Unit
  implicit def monoid[K[_]]: Monoid[AlwaysClean[K]] = monoidK.monoid
  implicit def monoidK: MonoidK[AlwaysClean] = new MonoidK[AlwaysClean] {
    def zero[K[_]]: AlwaysClean[K] = ()
    def append[K[_]](f1: AlwaysClean[K], f2: AlwaysClean[K]): AlwaysClean[K] = ()
  }

  trait CleanInterpreter[F[_[_], _], S[_[_]]] extends Interpreter[F, S, AlwaysClean] {
    def step0[K[_]: Applicative, A](f: F[K, A])(s: S[K]): (S[K], K[A])

    final def step[K[_]: Applicative, A](f: F[K, A])(s: S[K]): (S[K], AlwaysClean[K], K[A]) = step0(f)(s) match {
      case (s1, k) => (s1, (), k)
    }

    final def uncons[K[_]: Applicative](w: AlwaysClean[K])(s: S[K]): Option[(K[Unit], AlwaysClean[K], S[K])] = None
  }

  implicit def coproductInterpreter[G[_[_], _], H[_[_], _], T[_[_]], U[_[_]], X[_[_]], Y[_[_]]](implicit
    i1: Interpreter[G, T, X],
    i2: Interpreter[H, U, Y],
    m1: MonoidK[X],
    m2: MonoidK[Y]
  ): Interpreter[CoproductK[G, H, ?[_], ?], ProductK[T, U, ?[_]], ProductK[X, Y, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]
    type S[K[_]] = ProductK[T, U, K]
    type W[K[_]] = ProductK[X, Y, K]

    new Interpreter[F, S, W] {
      def step[K[_] : Applicative, A](f: F[K, A])(s: S[K]): (S[K], W[K], K[A]) = f.run match {
        case -\/(g) => i1.step(g)(s._1) match {
          case (t, x, k) => (s.update_1(t), ProductK(x, m2.zero[K]), k)
        }
        case \/-(h) => i2.step(h)(s._2) match {
          case (u, y, k) => (s.update_2(u), ProductK(m1.zero[K], y), k)
        }
      }

      def uncons[K[_] : Applicative](w: W[K])(s: S[K]): Option[(K[Unit], W[K], S[K])] = {
        i1.uncons(w._1)(s._1) match {
          case Some((k, x, t)) => Some((k, w.update_1(x), s.update_1(t)))
          case None => i2.uncons(w._2)(s._2) match {
            case Some((k, y, u)) => Some((k, w.update_2(y), s.update_2(u)))
            case None => None
          }
        }
      }
    }
  }

  implicit def coyonedaInterpreter[F[_[_], _], S[_[_]], W[_[_]]](implicit i: Interpreter[F, S, W]): Interpreter[CoyonedaK[F, ?[_], ?], S, W] =
    new Interpreter[CoyonedaK[F, ?[_], ?], S, W] {
      def step[K[_] : Applicative, A](c: CoyonedaK[F, K, A])(s: S[K]): (S[K], W[K], K[A]) = c match {
        case CoyonedaK.Pure(fa) => i.step(fa)(s)
        case CoyonedaK.Map(fx, f) => i.step(fx)(s) match { case (s1, w1, kx) => (s1, w1, Applicative[K].map(kx)(f)) }
      }

      def uncons[K[_] : Applicative](w: W[K])(s: S[K]): Option[(K[Unit], W[K], S[K])] = i.uncons(w)(s)
    }
}
