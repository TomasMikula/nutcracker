package nutcracker.util.free

import scala.language.higherKinds

import scalaz._
import scalaz.std.option._
import scalaz.syntax.applicative._

trait StateInterpreter[F[_[_], _]] {
  type State[K[_]]

  def step[K[_]: Applicative]: F[K, ?] ~> λ[A => scalaz.State[State[K], K[A]]]
  def uncons[K[_]: Applicative]: StateT[Option, State[K], K[Unit]]

  final def stepM[K[_]: Applicative, M[_]: Monad]: F[K, ?] ~> λ[A => StateT[M, State[K], K[A]]] =
    new (F[K, ?] ~> λ[A => StateT[M, State[K], K[A]]]) {
      val stepK = step[K]
      def apply[A](fa: F[K, A]): StateT[M, State[K], K[A]] = {
        val st = stepK(fa)
        StateT(s => st(s).point[M])
      }
    }

  def get[M[_]: Monad](): FreeK[F, ?] ~> StateT[M, State[FreeK[F, ?]], ?] = StateInterpreter(stepM[FreeK[F, ?], M], uncons[FreeK[F, ?]])

  def get[G[_[_], _], M[_]: Monad](
    ig: G ~>> M
  ): FreeK[CoproductK[G, F, ?[_], ?], ?] ~> StateT[M, State[FreeK[CoproductK[G, F, ?[_], ?], ?]], ?] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    implicit val fm: Monad[FreeK[H, ?]] = FreeK.freeKMonad[H]
    val trG: G[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], FreeK[H, A]]] =
      new (G[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], FreeK[H, A]]]) {
        def apply[A](ga: G[FreeK[H, ?], A]): StateT[M, State[FreeK[H, ?]], FreeK[H, A]] = {
          val ma = ig.apply[FreeK[H, ?], A](ga) map { FreeK.pure[H, A](_) }
          MonadTrans[StateT[?[_], State[FreeK[H, ?]], ?]].liftM(ma)
        }
      }
    val trF: F[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], FreeK[H, A]]] = stepM[FreeK[H, ?], M]
    val trH: H[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], FreeK[H, A]]] =
      CoproductK.transform[G, F, FreeK[H, ?], λ[A => StateT[M, State[FreeK[H, ?]], FreeK[H, A]]]](trG, trF)
    StateInterpreter[H, State, M](trH, uncons[FreeK[H, ?]])
  }
}

object StateInterpreter {

  type Aux[F0[_[_], _], S[_[_]]] = StateInterpreter[F0] { type State[K[_]] = S[K] }

  def apply[F[_[_], _], S[_[_]], M[_]](
    step: F[FreeK[F, ?], ?] ~> λ[A => StateT[M, S[FreeK[F, ?]], FreeK[F, A]]],
    uncons: StateT[Option, S[FreeK[F, ?]], FreeK[F, Unit]]
  )(implicit
    M: Monad[M]
  ): FreeK[F, ?] ~> StateT[M, S[FreeK[F, ?]], ?] = {

    def runUntilClean[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = {
      M.bind(runToCompletion(p)(s)){ case (s1, a) => M.map(runUntilClean1(s1)) {(_, a)} }
    }

    def runUntilClean1(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]]] = uncons(s) match {
      case None => s.point[M]
      case Some((s1, ku)) => M.bind(runToCompletion(ku)(s1)){ su => runUntilClean1(su._1) }
    }

    def runToCompletion[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = p match {
      case FreeK.Pure(a) => (s, a).point[M]
      case FreeK.Suspend(ffa) =>
        M.bind(step(ffa)(s)){ case (s1, ka) => runToCompletion(ka)(s1) }
      case bnd: FreeK.Bind[F, a1, A] =>
        M.bind(step(bnd.a)(s)){ case (s1, kx) => runToCompletion(kx >>= bnd.f)(s1) }
    }

    new (FreeK[F, ?] ~> StateT[M, S[FreeK[F, ?]], ?]) {
      def apply[A](fa: FreeK[F, A]): StateT[M, S[FreeK[F, ?]], A] = StateT(runUntilClean(fa))
    }
  }

  trait CleanStateInterpreter[F[_[_], _], S[_[_]]] extends StateInterpreter[F] {
    type State[K[_]] = S[K]

    final def uncons[K[_]: Applicative]: StateT[Option, S[K], K[Unit]] = StateT[Option, S[K], K[Unit]](s => None)
  }

  implicit def coproductInterpreter[G[_[_], _], H[_[_], _]](implicit
    i1: StateInterpreter[G],
    i2: StateInterpreter[H]
  ): StateInterpreter.Aux[CoproductK[G, H, ?[_], ?], ProductK[i1.State, i2.State, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]

    new StateInterpreter[F] {
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

  implicit def coyonedaInterpreter[F[_[_], _], S[_[_]]](implicit i: StateInterpreter.Aux[F, S]): StateInterpreter.Aux[CoyonedaK[F, ?[_], ?], S] =
    new StateInterpreter[CoyonedaK[F, ?[_], ?]] {
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
