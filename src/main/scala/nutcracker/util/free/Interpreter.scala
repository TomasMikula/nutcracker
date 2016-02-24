package nutcracker.util.free

import scala.language.higherKinds

import scala.annotation.tailrec
import scalaz._

trait Interpreter[F[_[_], _]] {
  type State[K[_]]
  type Dirty[K[_]]

  def step[K[_]: Applicative, A](f: F[K, A])(s: State[K]): (State[K], Dirty[K], K[A])
  def uncons[K[_]: Applicative](w: Dirty[K])(s: State[K]): Option[(K[Unit], Dirty[K], State[K])]

  def dirtyMonoidK: MonoidK[Dirty]

  private def mappend(a: Dirty[FreeK[F, ?]], b: Dirty[FreeK[F, ?]]): Dirty[FreeK[F, ?]] = dirtyMonoidK.append[FreeK[F, ?]](a, b)

  def runFree[A](
    s: State[FreeK[F, ?]],
    p: FreeK[F, A]
  ): (State[FreeK[F, ?]], A) = runFree(s, p, dirtyMonoidK.zero[FreeK[F, ?]])

  def runFreeUnit(
    s: State[FreeK[F, ?]],
    p: FreeK[F, Unit]
  ): State[FreeK[F, ?]] = runFreeUnit(s, p, dirtyMonoidK.zero[FreeK[F, ?]])

  @tailrec private def runFree[A](
    s: State[FreeK[F, ?]],
    p: FreeK[F, A],
    d: Dirty[FreeK[F, ?]]
  ): (State[FreeK[F, ?]], A) = p match {
    case FreeK.Pure(a) => (runFreeUnit(s, FreeK.Pure(()), d), a)
    case FreeK.Suspend(ffa) => step[FreeK[F, ?], A](ffa)(s) match {
      case (s1, d1, fa) => runFree(s1, fa, mappend(d, d1))
    }
    case bnd: FreeK.Bind[F, a1, A] => step[FreeK[F, ?], a1](bnd.a)(s) match {
      case (s1, d1, fx) => runFree(s1, fx >>= { bnd.f(_) }, mappend(d, d1))
    }
  }

  @tailrec private def runFreeUnit(
    s: State[FreeK[F, ?]],
    p: FreeK[F, Unit],
    d: Dirty[FreeK[F, ?]]
  ): State[FreeK[F, ?]] = p match {
    case FreeK.Pure(()) => uncons[FreeK[F, ?]](d)(s) match {
      case Some((cont, w1, s1)) => runFreeUnit(s1, cont, w1)
      case None => s
    }
    case FreeK.Suspend(ffu) => step[FreeK[F, ?], Unit](ffu)(s) match {
      case (s1, d1, fu) => runFreeUnit(s1, fu, mappend(d, d1))
    }
    case bnd: FreeK.Bind[F, a1, Unit] => step[FreeK[F, ?], a1](bnd.a)(s) match {
      case (s1, d1, fx) => runFreeUnit(s1, fx >>= { bnd.f(_) }, mappend(d, d1))
    }
  }
}

object Interpreter {

  type Aux[F0[_[_], _], S[_[_]], D[_[_]]] = Interpreter[F0] { type State[K[_]] = S[K]; type Dirty[K[_]] = D[K] }

  type ConstK[T, K[_]] = T
  implicit def constMonoidK[T: Monoid]: MonoidK[ConstK[T, ?[_]]] = new MonoidK[ConstK[T, ?[_]]] {
    def zero[K[_]]: ConstK[T, K] = Monoid[T].zero
    def append[K[_]](f1: ConstK[T, K], f2: ConstK[T, K]): ConstK[T, K] = Monoid[T].append(f1, f2)
  }

  type AlwaysClean[K[_]] = Unit
  implicit val alwaysCleanMonoidK: MonoidK[AlwaysClean] = new MonoidK[AlwaysClean] {
    def zero[K[_]]: AlwaysClean[K] = ()
    def append[K[_]](f1: AlwaysClean[K], f2: AlwaysClean[K]): AlwaysClean[K] = ()
  }

  trait CleanInterpreter[F[_[_], _], S[_[_]]] extends Interpreter[F] {
    type State[K[_]] = S[K]
    type Dirty[K[_]] = AlwaysClean[K]

    def step0[K[_]: Applicative, A](f: F[K, A])(s: S[K]): (S[K], K[A])

    final def step[K[_]: Applicative, A](f: F[K, A])(s: S[K]): (S[K], AlwaysClean[K], K[A]) = step0(f)(s) match {
      case (s1, k) => (s1, (), k)
    }

    final def uncons[K[_]: Applicative](w: AlwaysClean[K])(s: S[K]): Option[(K[Unit], AlwaysClean[K], S[K])] = None

    def dirtyMonoidK: MonoidK[Dirty] = alwaysCleanMonoidK
  }

  implicit def coproductInterpreter[G[_[_], _], H[_[_], _]](implicit
    i1: Interpreter[G],
    i2: Interpreter[H]
  ): Interpreter.Aux[CoproductK[G, H, ?[_], ?], ProductK[i1.State, i2.State, ?[_]], ProductK[i1.Dirty, i2.Dirty, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]

    new Interpreter[F] {
      type State[K[_]] = ProductK[i1.State, i2.State, K]
      type Dirty[K[_]] = ProductK[i1.Dirty, i2.Dirty, K]
      def step[K[_] : Applicative, A](f: F[K, A])(s: State[K]): (State[K], Dirty[K], K[A]) = f.run match {
        case -\/(g) => i1.step(g)(s._1) match {
          case (t, x, k) => (s.update_1(t), ProductK(x, i2.dirtyMonoidK.zero[K]), k)
        }
        case \/-(h) => i2.step(h)(s._2) match {
          case (u, y, k) => (s.update_2(u), ProductK(i1.dirtyMonoidK.zero[K], y), k)
        }
      }

      def uncons[K[_] : Applicative](w: Dirty[K])(s: State[K]): Option[(K[Unit], Dirty[K], State[K])] = {
        i1.uncons(w._1)(s._1) match {
          case Some((k, x, t)) => Some((k, w.update_1(x), s.update_1(t)))
          case None => i2.uncons(w._2)(s._2) match {
            case Some((k, y, u)) => Some((k, w.update_2(y), s.update_2(u)))
            case None => None
          }
        }
      }

      def dirtyMonoidK: MonoidK[Dirty] = ProductK.monoidK(i1.dirtyMonoidK, i2.dirtyMonoidK)
    }
  }

  implicit def coyonedaInterpreter[F[_[_], _], S[_[_]], W[_[_]]](implicit i: Interpreter.Aux[F, S, W]): Interpreter.Aux[CoyonedaK[F, ?[_], ?], S, W] =
    new Interpreter[CoyonedaK[F, ?[_], ?]] {
      type State[K[_]] = S[K]
      type Dirty[K[_]] = W[K]

      def step[K[_] : Applicative, A](c: CoyonedaK[F, K, A])(s: S[K]): (S[K], W[K], K[A]) = c match {
        case CoyonedaK.Pure(fa) => i.step(fa)(s)
        case CoyonedaK.Map(fx, f) => i.step(fx)(s) match { case (s1, w1, kx) => (s1, w1, Applicative[K].map(kx)(f)) }
      }

      def uncons[K[_] : Applicative](w: W[K])(s: S[K]): Option[(K[Unit], W[K], S[K])] = i.uncons(w)(s)

      def dirtyMonoidK: MonoidK[Dirty] = i.dirtyMonoidK
    }
}
