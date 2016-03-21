package nutcracker.util

import scala.language.higherKinds
import scalaz.{Id => _, _}
import scalaz.Id._
import scalaz.std.option._
import scalaz.syntax.applicative._

trait StateInterpreterT[M[_], F[_[_], _]] { self =>
  type State[K[_]]

  def step: StepT[M, F, State]
  def uncons[K[_]]: StateT[Option, State[K], List[K[Unit]]]

  final def :+:[G[_[_], _]](i2: StateInterpreterT[M, G])(implicit M: Functor[M]): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], ProductK[i2.State, State, ?[_]]] =
    StateInterpreterT.coproductInterpreter(i2, this)

  final def :+:[G[_[_], _], S[_[_]]](
    i2: StepT[M, G, S]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], ProductK[S, State, ?[_]]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    new StateInterpreterT[M, H] {
      type State[K[_]] = ProductK[S, self.State, K]
      def step: StepT[M, H, State] = i2 :+: self.step
      def uncons[K[_]]: StateT[Option, State[K], List[K[Unit]]] =
        self.uncons[K].zoom(ProductK.rightLensZ[S, self.State, K])
    }
  }

  def get(implicit M: Monad[M]): FreeK[F, ?] ~> StateT[M, State[FreeK[F, ?]], ?] = StateInterpreterT(step.run.papply[FreeK[F, ?]], uncons[FreeK[F, ?]])

  def get[G[_[_], _]](
    ig: G ~>> M
  )(implicit
    M: Monad[M]
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
    val trF: F[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]] = step.run.papply[FreeK[H, ?]]
    val trH: H[FreeK[H, ?], ?] ~> λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]] =
      CoproductK.transform[G, F, FreeK[H, ?], λ[A => StateT[M, State[FreeK[H, ?]], (A, List[FreeK[H, Unit]])]]](trG, trF)
    StateInterpreterT[M, H, State](trH, uncons[FreeK[H, ?]])
  }
}

object StateInterpreterT {

  type Aux[M[_], F[_[_], _], S[_[_]]] = StateInterpreterT[M, F] { type State[K[_]] = S[K] }

  type StateInterpreter[F[_[_], _]]  = StateInterpreterT[Id, F]
  object StateInterpreter {
    type Aux[F[_[_], _], S[_[_]]] = StateInterpreterT.Aux[Id, F, S]
  }

  def apply[M[_], F[_[_], _], S[_[_]]](
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

  def coproductInterpreter[M[_], G[_[_], _], H[_[_], _]](
    i1: StateInterpreterT[M, G],
    i2: StateInterpreterT[M, H]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, H, ?[_], ?], ProductK[i1.State, i2.State, ?[_]]] = {

    type F[K[_], A] = CoproductK[G, H, K, A]

    new StateInterpreterT[M, F] {
      type State[K[_]] = ProductK[i1.State, i2.State, K]
      def step: StepT[M, F, State] = i1.step :+: i2.step

      def uncons[K[_]]: StateT[Option, State[K], List[K[Unit]]] = {
        val uncons1 = i1.uncons[K].zoom(ProductK.leftLensZ[i1.State, i2.State, K])
        val uncons2 = i2.uncons[K].zoom(ProductK.rightLensZ[i1.State, i2.State, K])
        StateT(s => uncons1(s).orElse(uncons2(s)))
      }
    }
  }

  implicit def coyonedaInterpreter[M[_], F[_[_], _], S[_[_]]](implicit
    i: StateInterpreterT.Aux[M, F, S],
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoyonedaK[F, ?[_], ?], S] =
    new StateInterpreterT[M, CoyonedaK[F, ?[_], ?]] {
      type State[K[_]] = S[K]

      def step: StepT[M, CoyonedaK[F, ?[_], ?], S] =
        StepT[M, CoyonedaK[F, ?[_], ?], S](new (CoyonedaK[F, ?[_], ?] ~~> λ[(K[_], A) => StateT[M, S[K], (A, List[K[Unit]])]]) {
          override def apply[K[_], A](c: CoyonedaK[F, K, A]): StateT[M, S[K], (A, List[K[Unit]])] = c match {
            case CoyonedaK.Pure(fa) => i.step.apply(fa)
            case CoyonedaK.Map(fx, f) => i.step.apply(fx) map { case (x, ku) => (f(x), ku) }
          }
        })

      def uncons[K[_]]: StateT[Option, S[K], List[K[Unit]]] = i.uncons[K]
    }
}


final case class StepT[M[_], F[_[_], _], S[_[_]]](run: F ~~> λ[(K[_], A) => StateT[M, S[K], (A, List[K[Unit]])]]) extends AnyVal { self =>

  final def apply[K[_], A](f: F[K, A]): StateT[M, S[K], (A, List[K[Unit]])] = run(f)

  def :+:[G[_[_], _], T[_[_]]](
    that: StepT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StepT[M, CoproductK[G, F, ?[_], ?], ProductK[T, S, ?[_]]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K[_]] = ProductK[T, S, K]
    val gLens = ProductK.leftLensZK[T, S]
    val fLens = ProductK.rightLensZK[T, S]
    StepT[M, H, U](new (H ~~> λ[(K[_], A) => StateT[M, U[K], (A, List[K[Unit]])]]) {
      override def apply[K[_], A](h: H[K, A]): StateT[M, ProductK[T, S, K], (A, List[K[Unit]])] =
        h.run match {
          case -\/(g) => that(g).zoom(gLens[K])
          case \/-(f) => self(f).zoom(fLens[K])
        }
    })
  }

  final def :+:[G[_[_], _]](
    ig: StateInterpreterT[M, G]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], ProductK[ig.State, S, ?[_]]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    new StateInterpreterT[M, H] {
      type State[K[_]] = ProductK[ig.State, S, K]
      def step: StepT[M, H, State] = ig.step :+: self
      def uncons[K[_]]: StateT[Option, State[K], List[K[Unit]]] =
        ig.uncons[K].zoom(ProductK.leftLensZ[ig.State, S, K])
    }
  }
}

object StepT {
  type Step[F[_[_], _], S[_[_]]] = StepT[Id, F, S]
  object Step {
    def apply[F[_[_], _], S[_[_]]](run: F ~~> λ[(K[_], A) => State[S[K], (A, List[K[Unit]])]]): Step[F, S] = StepT[Id, F, S](run)
  }
}