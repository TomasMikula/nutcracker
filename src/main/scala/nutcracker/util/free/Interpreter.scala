package nutcracker.util.free

import scala.language.higherKinds

import scala.annotation.tailrec
import scalaz._
import scalaz.syntax.applicative._

trait Interpreter[F[_[_], _]] {
  type State[K[_]]

  def step[K[_]: Applicative, A](f: F[K, A])(s: State[K]): (State[K], K[A])
  def uncons[K[_]: Applicative](s: State[K]): Option[(K[Unit], State[K])]

  @tailrec final def runFree[A](
    s: State[FreeK[F, ?]],
    p: FreeK[F, A]
  ): (State[FreeK[F, ?]], A) = p match {
    case FreeK.Pure(a) => (runFreeUnit(s, FreeK.Pure(())), a)
    case FreeK.Suspend(ffa) => step[FreeK[F, ?], A](ffa)(s) match {
      case (s1, ka) => (runFree(s1, ka))
    }
    case bnd: FreeK.Bind[F, a1, A] => step[FreeK[F, ?], a1](bnd.a)(s) match {
      case (s1, kx) => runFree(s1, kx >>= bnd.f)
    }
  }

  @tailrec final def runFreeUnit(
    s: State[FreeK[F, ?]],
    p: FreeK[F, Unit]
  ): State[FreeK[F, ?]] = p match {
    case FreeK.Pure(()) => uncons[FreeK[F, ?]](s) match {
      case Some((cont, s1)) => runFreeUnit(s1, cont)
      case None => s
    }
    case FreeK.Suspend(ffu) => step[FreeK[F, ?], Unit](ffu)(s) match {
      case (s1, ku) => runFreeUnit(s1, ku)
    }
    case bnd: FreeK.Bind[F, a1, Unit] => step[FreeK[F, ?], a1](bnd.a)(s) match {
      case (s1, kx) => runFreeUnit(s1, kx >>= bnd.f)
    }
  }
}

object Interpreter {

  type Aux[F0[_[_], _], S[_[_]]] = Interpreter[F0] { type State[K[_]] = S[K] }

  type ConstK[T, K[_]] = T
  implicit def constMonoidK[T: Monoid]: MonoidK[ConstK[T, ?[_]]] = new MonoidK[ConstK[T, ?[_]]] {
    def zero[K[_]]: ConstK[T, K] = Monoid[T].zero
    def append[K[_]](f1: ConstK[T, K], f2: ConstK[T, K]): ConstK[T, K] = Monoid[T].append(f1, f2)
  }

  trait CleanInterpreter[F[_[_], _], S[_[_]]] extends Interpreter[F] {
    type State[K[_]] = S[K]

    final def uncons[K[_]: Applicative](s: S[K]): Option[(K[Unit], S[K])] = None
  }

  implicit def coproductInterpreter[G[_[_], _], H[_[_], _]](implicit
    i1: Interpreter[G],
    i2: Interpreter[H]
  ): Interpreter.Aux[CoproductK[G, H, ?[_], ?], ProductK[i1.State, i2.State, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]

    new Interpreter[F] {
      type State[K[_]] = ProductK[i1.State, i2.State, K]
      def step[K[_] : Applicative, A](f: F[K, A])(s: State[K]): (State[K], K[A]) = f.run match {
        case -\/(g) => i1.step(g)(s._1) match {
          case (t, a) => (s.update_1(t), a)
        }
        case \/-(h) => i2.step(h)(s._2) match {
          case (u, a) => (s.update_2(u), a)
        }
      }

      def uncons[K[_] : Applicative](s: State[K]): Option[(K[Unit], State[K])] = {
        i1.uncons(s._1) match {
          case Some((k, t)) => Some((k, s.update_1(t)))
          case None => i2.uncons(s._2) match {
            case Some((k, u)) => Some((k, s.update_2(u)))
            case None => None
          }
        }
      }
    }
  }

  implicit def coyonedaInterpreter[F[_[_], _], S[_[_]]](implicit i: Interpreter.Aux[F, S]): Interpreter.Aux[CoyonedaK[F, ?[_], ?], S] =
    new Interpreter[CoyonedaK[F, ?[_], ?]] {
      type State[K[_]] = S[K]

      def step[K[_] : Applicative, A](c: CoyonedaK[F, K, A])(s: S[K]): (S[K], K[A]) = c match {
        case CoyonedaK.Pure(fa) => i.step(fa)(s)
        case CoyonedaK.Map(fx, f) => i.step(fx)(s) match { case (s1, kx) => (s1, kx map f) }
      }

      def uncons[K[_] : Applicative](s: S[K]): Option[(K[Unit], S[K])] = i.uncons(s)

    }
}
