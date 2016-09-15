package nutcracker.util

import scala.language.higherKinds
import scalaz.{BindRec, Functor, Lens, Monad, StateT, \/, ~>}
import scalaz.Id._
import scalaz.std.option._
import scalaz.syntax.applicative._
import scalaz.syntax.either._
import nutcracker.util.KList._

trait StateInterpreterT[M[_], F[_[_], _], S[_]] { self =>
  def step: StepT[M, F, S]
  def uncons: Uncons[S]

  final def :&&:[G[_[_], _], T[_]](that: StateInterpreterT[M, G, T])(implicit M: Functor[M]): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], Cons[T, Just[S, ?], ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K] = Cons[T, Just[S, ?], K]

    new StateInterpreterT[M, H, U] {
      def step: StepT[M, H, U] = that.step :&&: self.step

      def uncons: Uncons[U] = {
        val uncons1 = that.uncons.zoomOut[U](implicitly[ValA[λ[K => Lens[U[K], T[K]]]]])
        val uncons2 = self.uncons.zoomOut[U](implicitly[ValA[λ[K => Lens[U[K], S[K]]]]])
        uncons1 orElse uncons2
      }
    }
  }

  final def :&&:[G[_[_], _], T[_]](
    i2: StepT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], Cons[T, Just[S, ?], ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K] = Cons[T, Just[S, ?], K]
    new StateInterpreterT[M, H, U] {
      def step: StepT[M, H, U] = i2 :&&: self.step
      def uncons: Uncons[U] = self.uncons.zoomOut[U](implicitly[ValA[λ[K => Lens[U[K], S[K]]]]])
    }
  }

  final def ::[G[_[_], _]](that: StateInterpreterT[M, G, S]): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], S] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    new StateInterpreterT[M, H, S] {
      def step: StepT[M, H, S] = that.step :: self.step
      def uncons: Uncons[S] = self.uncons
    }
  }

  final def :>>:[G[_[_], _]](
    ig: G ≈>> M
  )(implicit
    M: Monad[M]
  ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], S] =
    new StateInterpreterT[M, CoproductK[G, F, ?[_], ?], S] {
      def step = ig :>>: self.step
      def uncons = self.uncons
    }

  def hoist[N[_]](mn: M ~> N)(implicit M: Monad[M], N: Monad[N]): StateInterpreterT[N, F, S] =
    new StateInterpreterT[N, F, S] {
      def step: StepT[N, F, S] = self.step.hoist(mn)
      def uncons: Uncons[S] = self.uncons
    }

  def hoistId[N[_]](implicit N: Monad[N], ev: this.type <:< StateInterpreterT[Id, F, S]): StateInterpreterT[N, F, S] =
    ev(this).hoist(idToM[N])

  def freeInstance(implicit M0: Monad[M], M1: BindRec[M]): FreeK[F, ?] ~> StateT[M, S[FreeK[F, Unit]], ?] =
    StateInterpreterT.freeInstance(step, uncons)
}

object StateInterpreterT {
  import scala.language.implicitConversions  

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

    def runUntilClean1(s: S[FreeK[F, Unit]]): M[S[FreeK[F, Unit]]] =
      M1.tailrecM(s)(s => uncons1(s) match {
        case None => s.right.point[M]
        case Some((s1, ku)) => M0.map(runToCompletionU(ku)(s1)){ _.left }
      })

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
      M1.tailrecM((ps, s))(go)
    }

    new (FreeK[F, ?] ~> StateT[M, S[FreeK[F, Unit]], ?]) {
      def apply[A](fa: FreeK[F, A]): StateT[M, S[FreeK[F, Unit]], A] = StateT(runUntilClean(fa))
    }
  }

  final case class Ops[M[_], F[_[_], _], S[_] <: KList[_]](self: StateInterpreterT[M, F, S]) extends AnyVal {

    def :&:[G[_[_], _], T[_]](
      that: StepT[M, G, T]
    )(implicit
      M: Functor[M]
    ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], Cons[T, S, ?]] = {
      type H[K[_], A] = CoproductK[G, F, K, A]
      type U[K] = Cons[T, S, K]

      new StateInterpreterT[M, H, U] {
        def step: StepT[M, H, U] = that :&: self.step
        def uncons: Uncons[U] =
          self.uncons.zoomOut[U](implicitly[ValA[λ[K => Lens[U[K], S[K]]]]])
      }
    }

    def :&:[G[_[_], _], T[_]](that: StateInterpreterT[M, G, T])(implicit M: Functor[M]): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], Cons[T, S, ?]] = {
      type H[K[_], A] = CoproductK[G, F, K, A]
      type U[K] = Cons[T, S, K]

      new StateInterpreterT[M, H, U] {
        def step: StepT[M, H, U] = that.step :&: self.step

        def uncons: Uncons[U] = {
          val uncons1 = that.uncons.zoomOut[U](implicitly[ValA[λ[K => Lens[U[K], T[K]]]]])
          val uncons2 = self.uncons.zoomOut[U](implicitly[ValA[λ[K => Lens[U[K], S[K]]]]])
          uncons1 orElse uncons2
        }
      }
    }
  }

  implicit def toOps[M[_], F[_[_], _], S[_] <: KList[_]](i: StateInterpreterT[M, F, S]): Ops[M, F, S] =
    Ops(i)
}
