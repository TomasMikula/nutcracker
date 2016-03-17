package nutcracker.util.free

import scala.language.higherKinds

import scalaz._
import scalaz.std.option._
import scalaz.syntax.applicative._

trait StateInterpreter[F[_[_], _]] {
  type State[K[_]]

  def step: F ~~> λ[(K[_], A) => scalaz.State[State[K], (A, List[K[Unit]])]]
  def uncons[K[_]]: StateT[Option, State[K], List[K[Unit]]]

  final def stepM[M[_]: Monad]: F[?[_], ?] ~~> λ[(K[_], A) => StateT[M, State[K], (A, List[K[Unit]])]] =
    new (F[?[_], ?] ~~> λ[(K[_], A) => StateT[M, State[K], (A, List[K[Unit]])]]) {
      def apply[K[_], A](fa: F[K, A]): StateT[M, State[K], (A, List[K[Unit]])] = {
        val st = step(fa)
        StateT(s => st(s).point[M])
      }
    }

  final def compose[G[_[_], _]](i2: StateInterpreter[G]): StateInterpreter.Aux[CoproductK[G, F, ?[_], ?], ProductK[i2.State, State, ?[_]]] =
    StateInterpreter.coproductInterpreter(i2, this)

  final def :+:[G[_[_], _]](i2: StateInterpreter[G]) = compose(i2)

  def get[M[_]: Monad](): FreeK[F, ?] ~> StateT[M, State[FreeK[F, ?]], ?] = StateInterpreter(stepM[M].papply[FreeK[F, ?]], uncons[FreeK[F, ?]])

  def get[G[_[_], _], M[_]: Monad](
    ig: G ~>> M
  ): FreeK[CoproductK[G, F, ?[_], ?], ?] ~> StateT[M, State[FreeK[CoproductK[G, F, ?[_], ?], ?]], ?] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    implicit val fm: Monad[FreeK[H, ?]] = FreeK.freeKMonad[H]
    val trG: G[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]] =
      new (G[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]]) {
        def apply[A](ga: G[FreeK[H, ?], A]): StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])] = {
          val ma = ig.apply[FreeK[H, ?], A](ga) map { (_, List.empty[FreeK[H, Unit]]) }
          MonadTrans[StateT[?[_], State[FreeK[H, ?]], ?]].liftM(ma)
        }
      }
    val trF: F[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]] = stepM[M].papply[FreeK[H, ?]]
    val trH: H[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]] =
      CoproductK.transform[G, F, FreeK[H, ?], λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]]](trG, trF)
    StateInterpreter[H, State, M](trH, uncons[FreeK[H, ?]])
  }
}

object StateInterpreter {

  type Aux[F0[_[_], _], S[_[_]]] = StateInterpreter[F0] { type State[K[_]] = S[K] }

  def apply[F[_[_], _], S[_[_]], M[_]](
    step: F[FreeK[F, ?], ?] ~> λ[A => StateT[M, S[FreeK[F, ?]], (A, List[FreeK[F, Unit]])]],
    uncons: StateT[Option, S[FreeK[F, ?]], List[FreeK[F, Unit]]]
  )(implicit
    M: Monad[M]
  ): FreeK[F, ?] ~> StateT[M, S[FreeK[F, ?]], ?] = {

    def runUntilClean[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = {
      M.bind(runToCompletion(p)(s)){ case (s1, a) => M.map(runUntilClean1(s1)) {(_, a)} }
    }

    def runUntilClean1(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]]] = uncons(s) match {
      case None => s.point[M]
      case Some((s1, ku)) => M.bind(runToCompletionU(ku)(s1)){ s2 => runUntilClean1(s2) }
    }

    def runToCompletion[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = p match {
      case FreeK.Pure(a) => (s, a).point[M]
      case FreeK.Suspend(ffa) =>
        M.bind(step(ffa)(s)){ case (s1, (a, ku)) => runToCompletionU(ku)(s1) map { (_, a) } }
      case bnd: FreeK.Bind[F, a1, A] =>
        M.bind(step(bnd.a)(s)){ case (s1, (x, ku)) => M.bind(runToCompletion(bnd.f(x))(s1)) { case (s2, a) => runToCompletionU(ku)(s2) map { (_, a) } } }
    }

    def runToCompletionU(ps: List[FreeK[F, Unit]])(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]]] = ps match {
      case Nil => s.point[M]
      case k::ks => M.bind(runToCompletion(k)(s)) { case (s1, u) => runToCompletionU(ks)(s1) }
    }

    new (FreeK[F, ?] ~> StateT[M, S[FreeK[F, ?]], ?]) {
      def apply[A](fa: FreeK[F, A]): StateT[M, S[FreeK[F, ?]], A] = StateT(runUntilClean(fa))
    }
  }

  trait CleanStateInterpreter[F[_[_], _], S[_[_]]] extends StateInterpreter[F] {
    type State[K[_]] = S[K]

    final def uncons[K[_]]: StateT[Option, S[K], List[K[Unit]]] = StateT[Option, S[K], List[K[Unit]]](s => None)
  }

  def coproductInterpreter[G[_[_], _], H[_[_], _]](
    i1: StateInterpreter[G],
    i2: StateInterpreter[H]
  ): StateInterpreter.Aux[CoproductK[G, H, ?[_], ?], ProductK[i1.State, i2.State, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]

    new StateInterpreter[F] {
      type State[K[_]] = ProductK[i1.State, i2.State, K]
      def step: F ~~> λ[(K[_], A) => scalaz.State[State[K], (A, List[K[Unit]])]] = {
        val gLens = ProductK.leftLensZK[i1.State, i2.State]
        val hLens = ProductK.rightLensZK[i1.State, i2.State]
        new (F ~~> λ[(K[_], A) => scalaz.State[State[K], (A, List[K[Unit]])]]) {
          override def apply[K[_], A](f: F[K, A]): scalaz.State[State[K], (A, List[K[Unit]])] = f.run match {
            case -\/(g) => i1.step.apply(g).zoom(gLens[K])
            case \/-(h) => i2.step.apply(h).zoom(hLens[K])
          }
        }
      }

      def uncons[K[_]]: StateT[Option, State[K], List[K[Unit]]] = {
        val uncons1 = i1.uncons[K].zoom(ProductK.leftLensZ[i1.State, i2.State, K])
        val uncons2 = i2.uncons[K].zoom(ProductK.rightLensZ[i1.State, i2.State, K])
        StateT(s => uncons1(s).orElse(uncons2(s)))
      }
    }
  }

  implicit def coyonedaInterpreter[F[_[_], _], S[_[_]]](implicit i: StateInterpreter.Aux[F, S]): StateInterpreter.Aux[CoyonedaK[F, ?[_], ?], S] =
    new StateInterpreter[CoyonedaK[F, ?[_], ?]] {
      type State[K[_]] = S[K]

      def step: CoyonedaK[F, ?[_], ?] ~~> λ[(K[_], A) => scalaz.State[S[K], (A, List[K[Unit]])]] =
        new (CoyonedaK[F, ?[_], ?] ~~> λ[(K[_], A) => scalaz.State[S[K], (A, List[K[Unit]])]]) {
          override def apply[K[_], A](c: CoyonedaK[F, K, A]): scalaz.State[S[K], (A, List[K[Unit]])] = c match {
            case CoyonedaK.Pure(fa) => i.step.apply(fa)
            case CoyonedaK.Map(fx, f) => i.step.apply(fx) map { case (x, ku) => (f(x), ku) }
          }
        }

      def uncons[K[_]]: StateT[Option, S[K], List[K[Unit]]] = i.uncons[K]
    }
}
