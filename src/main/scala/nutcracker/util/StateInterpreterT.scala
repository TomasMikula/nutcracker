package nutcracker.util

import scala.language.higherKinds
import scalaz.{BindRec, Functor, Lens, Monad, StateT, \/, -\/, \/-, ~>}
import scalaz.Id._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.either._

trait StateInterpreterT[M[_], F[_[_], _]] { self =>
  type State[K[_]]

  def step: StepT[M, F, State]
  def uncons: Uncons[State]

  final def :*:[G[_[_], _]](i2: StateInterpreterT[M, G])(implicit M: Functor[M]): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], ProductK[i2.State, State, ?[_]]] =
    StateInterpreterT.coproductInterpreter(i2, this)

  final def :*:[G[_[_], _], S[_[_]]](
    i2: StepT[M, G, S]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], ProductK[S, State, ?[_]]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    new StateInterpreterT[M, H] {
      type State[K[_]] = ProductK[S, self.State, K]
      def step: StepT[M, H, State] = i2 :*: self.step
      def uncons: Uncons[State] = self.uncons.zoomOut[State](ProductK.rightLensZK[S, self.State])
    }
  }

  def :+:[G[_[_], _]](
    ig: G ≈>> M
  )(implicit
    M: Monad[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], State] = {
    val stepG: StepT[M, G, State] = StepT.lift[M, G, State](ig)
    new StateInterpreterT[M, CoproductK[G, F, ?[_], ?]] {
      type State[K[_]] = self.State[K]
      def step = stepG :+: self.step
      def uncons = self.uncons
    }
  }

  def hoist[N[_]](mn: M ~> N)(implicit M: Monad[M], N: Monad[N]): StateInterpreterT.Aux[N, F, State] =
    new StateInterpreterT[N, F] {
      type State[K[_]] = self.State[K]

      def step: StepT[N, F, State] = new StepT[N, F, State] {
        override def apply[K[_], A](f: F[K, A]): WriterStateT[N, Lst[K[Unit]], State[K], A] =
          WriterStateT(s => mn(self.step[K, A](f)(s)))
      }
      def uncons: Uncons[State] = self.uncons
    }

  def hoistId[N[_]](implicit N: Monad[N], ev: this.type <:< StateInterpreterT.Aux[Id, F, State]): StateInterpreterT.Aux[N, F, State] =
    ev(this).hoist(idToM[N])

  def freeInstance(implicit M0: Monad[M], M1: BindRec[M]): FreeK[F, ?] ~> StateT[M, State[FreeK[F, ?]], ?] =
    StateInterpreterT.freeInstance(step, uncons)
}

object StateInterpreterT {

  type Aux[M[_], F[_[_], _], S[_[_]]] = StateInterpreterT[M, F] { type State[K[_]] = S[K] }

  def freeInstance[M[_], F[_[_], _], S[_[_]]](
    step: StepT[M, F, S],
    uncons: Uncons[S]
  )(implicit
    M0: Monad[M],
    M1: BindRec[M]
  ): FreeK[F, ?] ~> StateT[M, S[FreeK[F, ?]], ?] = {
    val step1 = step.papply[FreeK[F, ?]]
    val uncons1 = uncons[FreeK[F, ?]]

    def runUntilClean[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] = {
      M0.bind(runToCompletion(p)(s)){ case (s1, a) => M0.map(runUntilClean1(s1)) {(_, a)} }
    }

    def runUntilClean1(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]]] = {
      def go(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]] \/ S[FreeK[F, ?]]] =
        uncons1(s) match {
          case None => s.right.point[M]
          case Some((s1, ku)) => M0.map(runToCompletionU(ku)(s1)){ _.left }
        }
      M1.tailrecM(go)(s)
    }

    def runToCompletion[A](p: FreeK[F, A])(s: S[FreeK[F, ?]]): M[(S[FreeK[F, ?]], A)] =
      M0.bind(p.foldMapN(step1).apply(s)) {
        case (ku, s1, a) => M0.map(runToCompletionU(ku)(s1)) { (_, a) }
      }

    def runToCompletionU(ps: Lst[FreeK[F, Unit]])(s: S[FreeK[F, ?]]): M[S[FreeK[F, ?]]] = {
      def go(a: (Lst[FreeK[F, Unit]], S[FreeK[F, ?]])): M[(Lst[FreeK[F, Unit]], S[FreeK[F, ?]]) \/ S[FreeK[F, ?]]] = {
        val (l, s) = a
        l.uncons match {
          case None => s.right.point[M]
          case Some((k, ks)) => M0.map(k.foldMapN(step1).apply(s)) { case (ks1, s1, ()) => (ks1 ++ ks, s1).left }
        }
      }
      M1.tailrecM(go)((ps, s))
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
      def step: StepT[M, F, State] = i1.step :*: i2.step

      def uncons: Uncons[State] = {
        val uncons1 = i1.uncons.zoomOut[State](ProductK.leftLensZK[i1.State, i2.State])
        val uncons2 = i2.uncons.zoomOut[State](ProductK.rightLensZK[i1.State, i2.State])
        uncons1 orElse uncons2
      }
    }
  }
}