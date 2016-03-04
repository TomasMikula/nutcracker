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

  def get(): FreeK[F, ?] ~> scalaz.State[State[FreeK[F, ?]], ?] = Interpreter(step[FreeK[F, ?]], uncons[FreeK[F, ?]])
}

object Interpreter {

  type Aux[F0[_[_], _], S[_[_]]] = Interpreter[F0] { type State[K[_]] = S[K] }

  def apply[F[_[_], _], S[_[_]]](
    step: F[FreeK[F, ?], ?] ~> λ[A => scalaz.State[S[FreeK[F, ?]], FreeK[F, A]]],
    uncons: StateT[Option, S[FreeK[F, ?]], FreeK[F, Unit]]
  ): FreeK[F, ?] ~> scalaz.State[S[FreeK[F, ?]], ?] = {

    def runUntilClean[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): (S[FreeK[F, ?]], A) = {
      val (s1, a) = runToCompletion(p)(s)
      (runUntilClean1(s1), a)
    }

    @tailrec def runUntilClean1(s: S[FreeK[F, ?]]): S[FreeK[F, ?]] = uncons(s) match {
      case None => s
      case Some((s1, ku)) => runUntilClean1(runToCompletion(ku)(s1)._1)
    }

    @tailrec def runToCompletion[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): (S[FreeK[F, ?]], A) = p match {
      case FreeK.Pure(a) => (s, a)
      case FreeK.Suspend(ffa) =>
        val (s1, ka) = step(ffa)(s)
        runToCompletion(ka)(s1)
      case bnd: FreeK.Bind[F, a1, A] =>
        val (s1, kx) = step(bnd.a)(s)
        runToCompletion(kx >>= bnd.f)(s1)
    }

    new (FreeK[F, ?] ~> scalaz.State[S[FreeK[F, ?]], ?]) {
      def apply[A](fa: FreeK[F, A]): scalaz.State[S[FreeK[F, ?]], A] = scalaz.State(runUntilClean(fa))
    }
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
      def step[K[_] : Applicative]: F[K, ?] ~> λ[A => scalaz.State[State[K], K[A]]] = {
        val gLens = ProductK.leftLensZ[i1.State, i2.State, K]
        val hLens = ProductK.rightLensZ[i1.State, i2.State, K]
        new (F[K, ?] ~> λ[A => scalaz.State[State[K], K[A]]]) {
          override def apply[A](f: F[K, A]): scalaz.State[State[K], K[A]] = f.run match {
            case -\/(g) => i1.step[K].apply(g).zoom(gLens)
            case \/-(h) => i2.step[K].apply(h).zoom(hLens)
          }
        }
      }

      def uncons[K[_] : Applicative]: StateT[Option, State[K], K[Unit]] = {
        val uncons1 = i1.uncons[K].zoom(ProductK.leftLensZ[i1.State, i2.State, K])
        val uncons2 = i2.uncons[K].zoom(ProductK.rightLensZ[i1.State, i2.State, K])
        StateT(s => uncons1(s).orElse(uncons2(s)))
      }
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
