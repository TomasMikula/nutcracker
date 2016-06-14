package nutcracker.util

import scala.language.higherKinds
import scalaz.{Functor, Monad, -\/, \/-, ~>}

abstract class StepT[M[_], F[_[_], _], S[_]] { self =>

  def apply[K[_], A](f: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A]

  def papply[K[_]]: F[K, ?] ~> WriterStateT[M, Lst[K[Unit]], S[K[Unit]], ?] =
    new (F[K, ?] ~> WriterStateT[M, Lst[K[Unit]], S[K[Unit]], ?]) {
      def apply[A](fa: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A] = self(fa)
    }

  def :*:[G[_[_], _], T[_]](
    that: StepT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StepT[M, CoproductK[G, F, ?[_], ?], ProductK[T, S, ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K] = ProductK[T, S, K]
    val gLens = ProductK.leftLensZK[T, S]
    val fLens = ProductK.rightLensZK[T, S]
    new StepT[M, H, U] {
      override def apply[K[_], A](h: H[K, A]): WriterStateT[M, Lst[K[Unit]], U[K[Unit]], A] =
        h.run match {
          case -\/(g) => that(g).zoomOut(gLens[K[Unit]])
          case \/-(f) => self(f).zoomOut(fLens[K[Unit]])
        }
    }
  }

  final def :*:[G[_[_], _]](
    ig: StateInterpreterT[M, G]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT.Aux[M, CoproductK[G, F, ?[_], ?], ProductK[ig.State, S, ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    new StateInterpreterT[M, H] {
      type State[K] = ProductK[ig.State, S, K]
      def step: StepT[M, H, State] = ig.step :*: self
      def uncons: Uncons[State] =
        ig.uncons.zoomOut[State](ProductK.leftLensZK[ig.State, S])
    }
  }

  def :+:[G[_[_], _]](that: StepT[M, G, S]): StepT[M, CoproductK[G, F, ?[_], ?], S] =
    new StepT[M, CoproductK[G, F, ?[_], ?], S] {
      def apply[K[_], A](ca: CoproductK[G, F, K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A] =
        WriterStateT(s => ca.run.fold(that(_), self(_))(s))
    }
}

object StepT {

  def lift[M[_]: Monad, F[_[_], _], S[_]](fm: F ≈>> M): StepT[M, F, S] =
    new StepT[M, F, S] {
      override def apply[K[_], A](f: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A] =
        WriterStateT.monadTrans[Lst[K[Unit]], S[K[Unit]]].liftM(fm(f))
    }
}
