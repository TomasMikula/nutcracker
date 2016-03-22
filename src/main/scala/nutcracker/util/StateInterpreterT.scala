package nutcracker.util

import scala.language.higherKinds
import scalaz.{Id => _, _}
import scalaz.Id._
import scalaz.std.option._
import scalaz.syntax.applicative._

trait StateInterpreterT[M[_], F[_[_], _]] { self =>
  type State[K[_]]

  def step: StepT[M, F, State]
  def uncons: Uncons[State]

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
      def uncons: Uncons[State] = self.uncons.zoomOut[State](ProductK.rightLensZK[S, self.State])
    }
  }

  def get(implicit M: Monad[M]): FreeK[F, ?] ~> StateT[M, State[FreeK[F, ?]], ?] =
    StateInterpreterT(step, uncons)

  def get[G[_[_], _]](
    ig: G ~>> M
  )(implicit
    M: Monad[M]
  ): FreeK[CoproductK[G, F, ?[_], ?], ?] ~> StateT[M, State[FreeK[CoproductK[G, F, ?[_], ?], ?]], ?] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    implicit val fm: Monad[FreeK[H, ?]] = FreeK.freeKMonad[H]
    val stepG: StepT[M, G, State] = StepT.lift[M, G, State](ig)
    val stepH: StepT[M, H, State] =
      StepT[M, H, State](CoproductK.transformKA[G, F, λ[(K[_], A) => StateT[M, State[K], (A, List[K[Unit]])]]](stepG.run, step.run))
    StateInterpreterT[M, H, State](stepH, uncons)
  }

  def hoist[N[_]](mn: M ~> N)(implicit M: Monad[M], N: Monad[N]): StateInterpreterT.Aux[N, F, State] =
    new StateInterpreterT[N, F] {
      type State[K[_]] = self.State[K]

      def step: StepT[N, F, State] = StepT(new (F ~~> λ[(K[_], A) => StateT[N, State[K], (A, List[K[Unit]])]]) {
        override def apply[K[_], A](f: F[K, A]): StateT[N, State[K], (A, List[K[Unit]])] =
          StateT(s => mn(self.step[K, A](f)(s)))
      })
      def uncons: Uncons[State] = self.uncons
    }
}

object StateInterpreterT {

  type Aux[M[_], F[_[_], _], S[_[_]]] = StateInterpreterT[M, F] { type State[K[_]] = S[K] }

  type StateInterpreter[F[_[_], _]]  = StateInterpreterT[Id, F]
  object StateInterpreter {
    type Aux[F[_[_], _], S[_[_]]] = StateInterpreterT.Aux[Id, F, S]
  }

  def apply[M[_], F[_[_], _], S[_[_]]](
    step: StepT[M, F, S],
    uncons: Uncons[S]
  )(implicit
    M: Monad[M]
  ): FreeK[F, ?] ~> StateT[M, S[FreeK[F, ?]], ?] = {
    val step1 = step.run.papply[FreeK[F, ?]]
    val uncons1 = uncons[FreeK[F, ?]]

    def runUntilClean[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = {
      M.bind(runToCompletion(p)(s)){ case (s1, a) => M.map(runUntilClean1(s1)) {(_, a)} }
    }

    def runUntilClean1(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]]] = uncons1(s) match {
      case None => s.point[M]
      case Some((s1, ku)) => M.bind(runToCompletionU(ku)(s1)){ s2 => runUntilClean1(s2) }
    }

    def runToCompletion[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = p match {
      case FreeK.Pure(a) => (s, a).point[M]
      case FreeK.Suspend(ffa) =>
        M.bind(step1(ffa)(s)){ case (s1, (a, ku)) => runToCompletionU(ku)(s1) map { (_, a) } }
      case bnd: FreeK.Bind[F, a1, A] =>
        M.bind(step1(bnd.a)(s)){ case (s1, (x, ku)) => M.bind(runToCompletion(bnd.f(x))(s1)) { case (s2, a) => runToCompletionU(ku)(s2) map { (_, a) } } }
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

      def uncons: Uncons[State] = {
        val uncons1 = i1.uncons.zoomOut[State](ProductK.leftLensZK[i1.State, i2.State])
        val uncons2 = i2.uncons.zoomOut[State](ProductK.rightLensZK[i1.State, i2.State])
        uncons1 orElse uncons2
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

      def uncons: Uncons[S] = i.uncons
    }
}


final case class StepT[M[_], F[_[_], _], S[_[_]]](
  run: F ~~> λ[(K[_], A) => StateT[M, S[K], (A, List[K[Unit]])]]
) extends AnyVal { self =>

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
      def uncons: Uncons[State] =
        ig.uncons.zoomOut[State](ProductK.leftLensZK[ig.State, S])
    }
  }
}

object StepT {

  def lift[M[_]: Monad, F[_[_], _], S[_[_]]](fm: F ~>> M): StepT[M, F, S] =
    StepT(new (F ~~> λ[(K[_], A) => StateT[M, S[K], (A, List[K[Unit]])]]) {
      override def apply[K[_], A](f: F[K, A]): StateT[M, S[K], (A, List[K[Unit]])] =
        MonadTrans[StateT[?[_], S[K], ?]].liftM(fm(f)).map(a => (a, List.empty[K[Unit]]))
    })

  type Step[F[_[_], _], S[_[_]]] = StepT[Id, F, S]
  object Step {
    def apply[F[_[_], _], S[_[_]]](run: F ~~> λ[(K[_], A) => State[S[K], (A, List[K[Unit]])]]): Step[F, S] = StepT[Id, F, S](run)
  }
}

final case class Uncons[S[_[_]]](run: ValK[λ[K[_] => StateT[Option, S[K], List[K[Unit]]]]]) extends AnyVal { self =>
  def apply[K[_]] = run[K]

  def zoomOut[T[_[_]]](f: ValK[λ[K[_] => Lens[T[K], S[K]]]]): Uncons[T] = {
    type StS[K[_]] = StateT[Option, S[K], List[K[Unit]]]
    type StT[K[_]] = StateT[Option, T[K], List[K[Unit]]]
    Uncons[T](run.map[StT](new (StS ≈> StT) {
      def apply[K[_]](sts: StS[K]): StT[K] = sts.zoom(f[K])
    }))
  }

  def orElse(that: Uncons[S]): Uncons[S] = Uncons[S](new ValK[λ[K[_] => StateT[Option, S[K], List[K[Unit]]]]] {
    override def compute[K[_]]: StateT[Option, S[K], List[K[Unit]]] =
      StateT(s => self[K](s).orElse(that[K](s)))
  })
}