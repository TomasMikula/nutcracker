package nutcracker.util.free

import scala.language.higherKinds

import scala.annotation.tailrec
import scalaz._
import scalaz.syntax.monoid._

trait Interpreter[F[_[_], _], S[_[_]], W[_[_]]] {

  def step[K[_]: Applicative, A](f: F[K, A])(s: S[K]): (S[K], W[K], K[A])
  def uncons[K[_]: Applicative](w: W[K])(s: S[K]): Option[(K[Unit], W[K], S[K])]
}

object Interpreter {
  trait Uncons[W[_[_]], S[_[_]]] {
    def uncons[K[_]](w: W[K])(s: S[K]): Option[(K[Unit], W[K])]
  }

  type AlwaysClean[K[_]] = Unit

  class FreeRunner[F[_[_], _], S[_[_]], W[_[_]]] {
    type K[A] = FreeK[F, A]
    type FF[A] = F[K, A]
    type SS = S[K]
    type WW = W[K]

    def runFree[A](
        i: Interpreter[F, S, W],
        s: SS,
        p: FreeK[F, A])(implicit
        W: Monoid[WW],
        F: Functor[FF]): (SS, A) = {
      runFree(i, s, p, W.zero)
    }

    @tailrec private def runFree[A](
        i: Interpreter[F, S, W],
        s: SS,
        p: FreeK[F, A],
        w: WW)(implicit
        W: Monoid[WW],
        F: Functor[FF]): (SS, A) = p.resume match {
      case \/-(a) => (runFreeUnit(i, s, FreeK.Pure(()), w), a)
      case -\/(ffa) => i.step[FreeK[F, ?], FreeK[F, A]](ffa)(s) match {
        case (s1, w1, fa) => runFree(i, s1, fa >>= { a => a }, w |+| w1)
      }
    }

    @tailrec private def runFreeUnit(
        i: Interpreter[F, S, W],
        s: SS,
        p: FreeK[F, Unit],
        w: WW)(implicit
        W: Monoid[WW],
        F: Functor[FF]): SS = p.resume match {
      case \/-(()) => i.uncons(w)(s) match {
        case Some((cont, w1, s1)) => runFreeUnit(i, s1, cont, w1)
        case None => s
      }
      case -\/(ffu) => i.step[FreeK[F, ?], FreeK[F, Unit]](ffu)(s) match {
        case (s1, w1, fu) => runFreeUnit(i, s1, fu >>= { u => u }, w |+| w1)
      }
    }
  }

  implicit def interpreter[G[_[_], _], H[_[_], _], T[_[_]], U[_[_]], X[_[_]], Y[_[_]]](implicit
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
          case (t, x, k) => (s.update_1(t), ProductK((x, m2.zero[K])), k)
        }
        case \/-(h) => i2.step(h)(s._2) match {
          case (u, y, k) => (s.update_2(u), ProductK((m1.zero[K], y)), k)
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
}
