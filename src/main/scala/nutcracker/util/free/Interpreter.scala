package nutcracker.util.free

import scala.language.higherKinds

import scala.annotation.tailrec
import scalaz._
import scalaz.std.option._
import scalaz.syntax.applicative._

trait Interpreter[F[_[_], _]] {
  type State[K[_]]

  def step[K[_]: Applicative]: F[K, ?] ~> λ[A => scalaz.State[State[K], K[A]]]
  def uncons[K[_]: Applicative]: StateT[Option, State[K], K[Unit]]

  def stepF: F[FreeK[F, ?], ?] ~> λ[A => scalaz.State[State[FreeK[F, ?]], FreeK[F, A]]] = step[FreeK[F, ?]]
  def unconsF: StateT[Option, State[FreeK[F, ?]], FreeK[F, Unit]] = uncons[FreeK[F, ?]]

  def runFree[A](p: FreeK[F, A]): scalaz.State[State[FreeK[F, ?]], A] = scalaz.State(runUntilClean(p))

  private def runUntilClean[A](p: FreeK[F, A])(s: State[FreeK[F, ?]]): (State[FreeK[F, ?]], A) = {
    val (s1, a) = runToCompletion(p)(s)
    (runUntilClean1(s1), a)
  }

  @tailrec private def runUntilClean1(s: State[FreeK[F, ?]]): State[FreeK[F, ?]] = unconsF(s) match {
    case None => s
    case Some((s1, ku)) => runUntilClean1(runToCompletion(ku)(s1)._1)
  }

  @tailrec private def runToCompletion[A](p: FreeK[F, A])(s: State[FreeK[F, ?]]): (State[FreeK[F, ?]], A) = p match {
    case FreeK.Pure(a) => (s, a)
    case FreeK.Suspend(ffa) =>
      val (s1, ka) = stepF(ffa)(s)
      runToCompletion(ka)(s1)
    case bnd: FreeK.Bind[F, a1, A] =>
      val (s1, kx) = stepF(bnd.a)(s)
      runToCompletion(kx >>= bnd.f)(s1)
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

    final def uncons[K[_]: Applicative]: StateT[Option, S[K], K[Unit]] = StateT[Option, S[K], K[Unit]](s => None)
  }

  implicit def coproductInterpreter[G[_[_], _], H[_[_], _]](implicit
    i1: Interpreter[G],
    i2: Interpreter[H]
  ): Interpreter.Aux[CoproductK[G, H, ?[_], ?], ProductK[i1.State, i2.State, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]

    new Interpreter[F] {
      type State[K[_]] = ProductK[i1.State, i2.State, K]
      def step[K[_] : Applicative]: F[K, ?] ~> λ[A => scalaz.State[State[K], K[A]]] = new (F[K, ?] ~> λ[A => scalaz.State[State[K], K[A]]]) {
        override def apply[A](f: F[K, A]): scalaz.State[State[K], K[A]] = f.run match {
          case -\/(g) => scalaz.State(s => i1.step[K].apply(g)(s._1) match {
            case (t, a) => (s.update_1(t), a)
          })
          case \/-(h) => scalaz.State(s => i2.step[K].apply(h)(s._2) match {
            case (u, a) => (s.update_2(u), a)
          })
        }
      }

      def uncons[K[_] : Applicative]: StateT[Option, State[K], K[Unit]] = StateT(s =>
        i1.uncons[K].apply(s._1).map({ case (t, k) => (s.update_1(t), k) }).orElse(
        i2.uncons[K].apply(s._2).map({ case (u, k) => (s.update_2(u), k) }))
      )
    }
  }

  implicit def coyonedaInterpreter[F[_[_], _], S[_[_]]](implicit i: Interpreter.Aux[F, S]): Interpreter.Aux[CoyonedaK[F, ?[_], ?], S] =
    new Interpreter[CoyonedaK[F, ?[_], ?]] {
      type State[K[_]] = S[K]

      def step[K[_] : Applicative]: CoyonedaK[F, K, ?] ~> λ[A => scalaz.State[S[K], K[A]]] = new (CoyonedaK[F, K, ?] ~> λ[A => scalaz.State[S[K], K[A]]]) {
        override def apply[A](c: CoyonedaK[F, K, A]): scalaz.State[S[K], K[A]] = c match {
          case CoyonedaK.Pure(fa) => i.step[K].apply(fa)
          case CoyonedaK.Map(fx, f) => i.step[K].apply(fx) map { kx => kx map f }
        }
      }

      def uncons[K[_] : Applicative]: StateT[Option, S[K], K[Unit]] = i.uncons[K]
    }
}
