package nutcracker.util

import scala.language.higherKinds
import scalaz.{BindRec, Functor, Lens, Monad, StateT, \/, -\/, \/-, ~>}
import scalaz.Id._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.either._
import nutcracker.util.KList._

trait StateInterpreterT[M[_], F[_[_], _]] { self =>
  type State[K]

  def step: StepT[M, F, State]
  def uncons: Uncons[State]

  final def :&&:[G[_[_], _]](that: StateInterpreterT[M, G])(implicit M: Functor[M]): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], Cons[that.State, Just[State, ?], ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]

    new StateInterpreterT[M, H] {
      type State[K] = Cons[that.State, Just[self.State, ?], K]

      def step: StepT[M, H, State] = that.step :&&: self.step

      def uncons: Uncons[State] = {
        val uncons1 = that.uncons.zoomOut[State](implicitly[ValA[λ[K => Lens[Cons[that.State, Just[self.State, ?], K], that.State[K]]]]])
        val uncons2 = self.uncons.zoomOut[State](implicitly[ValA[λ[K => Lens[Cons[that.State, Just[self.State, ?], K], self.State[K]]]]])
        uncons1 orElse uncons2
      }
    }
  }

  final def :&&:[G[_[_], _], S[_]](
    i2: StepT[M, G, S]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], Cons[S, Just[State, ?], ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    new StateInterpreterT[M, H] {
      type State[K] = Cons[S, Just[self.State, ?], K]
      def step: StepT[M, H, State] = i2 :&&: self.step
      def uncons: Uncons[State] = self.uncons.zoomOut[State](implicitly[ValA[λ[K => Lens[Cons[S, Just[self.State, ?], K], self.State[K]]]]])
    }
  }

  final def ::[G[_[_], _]](
    ig: G ≈>> M
  )(implicit
    M: Monad[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], State] = {
    val stepG: StepT[M, G, State] = StepT.lift[M, G, State](ig)
    new StateInterpreterT[M, CoproductK[G, F, ?[_], ?]] {
      type State[K] = self.State[K]
      def step = stepG :: self.step
      def uncons = self.uncons
    }
  }

  def hoist[N[_]](mn: M ~> N)(implicit M: Monad[M], N: Monad[N]): StateInterpreterT.Aux[N, F, State] =
    new StateInterpreterT[N, F] {
      type State[K] = self.State[K]

      def step: StepT[N, F, State] = self.step.hoist(mn)
      def uncons: Uncons[State] = self.uncons
    }

  def hoistId[N[_]](implicit N: Monad[N], ev: this.type <:< StateInterpreterT.Aux[Id, F, State]): StateInterpreterT.Aux[N, F, State] =
    ev(this).hoist(idToM[N])

  def freeInstance(implicit M0: Monad[M], M1: BindRec[M]): FreeK[F, ?] ~> StateT[M, State[FreeK[F, Unit]], ?] =
    StateInterpreterT.freeInstance(step, uncons)
}

object StateInterpreterT {
  import scala.language.implicitConversions  

  type Aux[M[_], F[_[_], _], S[_]] = StateInterpreterT[M, F] { type State[K] = S[K] }

  def freeInstance[M[_], F[_[_], _], S[_]](
    step: StepT[M, F, S],
    uncons: Uncons[S]
  )(implicit
    M0: Monad[M],
    M1: BindRec[M]
  ): FreeK[F, ?] ~> StateT[M, S[FreeK[F, Unit]], ?] = {
    val step1 = step.papply[FreeK[F, ?]]
    val uncons1 = uncons[FreeK[F, Unit]]

    def runUntilClean[A](p: FreeK[F, A])(s: S[FreeK[F, Unit]]): M[(S[FreeK[F, Unit]], A)] = {
      M0.bind(runToCompletion(p)(s)){ case (s1, a) => M0.map(runUntilClean1(s1)) {(_, a)} }
    }

    def runUntilClean1(s: S[FreeK[F, Unit]]): M[S[FreeK[F, Unit]]] = {
      def go(s: S[FreeK[F, Unit]]): M[S[FreeK[F, Unit]] \/ S[FreeK[F, Unit]]] =
        uncons1(s) match {
          case None => s.right.point[M]
          case Some((s1, ku)) => M0.map(runToCompletionU(ku)(s1)){ _.left }
        }
      M1.tailrecM(go)(s)
    }

    def runToCompletion[A](p: FreeK[F, A])(s: S[FreeK[F, Unit]]): M[(S[FreeK[F, Unit]], A)] =
      M0.bind(p.foldMapN(step1).apply(s)) {
        case (ku, s1, a) => M0.map(runToCompletionU(ku)(s1)) { (_, a) }
      }

    def runToCompletionU(ps: Lst[FreeK[F, Unit]])(s: S[FreeK[F, Unit]]): M[S[FreeK[F, Unit]]] = {
      def go(a: (Lst[FreeK[F, Unit]], S[FreeK[F, Unit]])): M[(Lst[FreeK[F, Unit]], S[FreeK[F, Unit]]) \/ S[FreeK[F, Unit]]] = {
        val (l, s) = a
        l.uncons match {
          case None => s.right.point[M]
          case Some((k, ks)) => M0.map(k.foldMapN(step1).apply(s)) { case (ks1, s1, ()) => (ks1 ++ ks, s1).left }
        }
      }
      M1.tailrecM(go)((ps, s))
    }

    new (FreeK[F, ?] ~> StateT[M, S[FreeK[F, Unit]], ?]) {
      def apply[A](fa: FreeK[F, A]): StateT[M, S[FreeK[F, Unit]], A] = StateT(runUntilClean(fa))
    }
  }

  final case class Ops[M[_], F[_[_], _], S[_] <: KList[_]](self: StateInterpreterT.Aux[M, F, S]) extends AnyVal {

    def :&:[G[_[_], _]](that: StateInterpreterT[M, G])(implicit M: Functor[M]): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], Cons[that.State, S, ?]] = {
      type H[K[_], A] = CoproductK[G, F, K, A]

      new StateInterpreterT[M, H] {
        type State[K] = Cons[that.State, S, K]

        def step: StepT[M, H, State] = that.step :&: self.step

        def uncons: Uncons[State] = {
          val uncons1 = that.uncons.zoomOut[State](implicitly[ValA[λ[K => Lens[State[K], that.State[K]]]]])
          val uncons2 = self.uncons.zoomOut[State](implicitly[ValA[λ[K => Lens[State[K], self.State[K]]]]])
          uncons1 orElse uncons2
        }
      }
    }
  }

  implicit def toOps[M[_], F[_[_], _], S[_] <: KList[_]](i: StateInterpreterT.Aux[M, F, S]): Ops[M, F, S] =
    Ops(i)
}
