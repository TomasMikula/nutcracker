package nutcracker.util

import scala.language.higherKinds
import scalaz.{Functor, Monad, -\/, \/-, ~>}
import nutcracker.util.KPair._

abstract class StepT[M[_], F[_[_], _], S[_[_]]] { self =>

  def apply[K[_], A](f: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K], A]

  def papply[K[_]]: F[K, ?] ~> WriterStateT[M, Lst[K[Unit]], S[K], ?] =
    new (F[K, ?] ~> WriterStateT[M, Lst[K[Unit]], S[K], ?]) {
      def apply[A](fa: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K], A] = self(fa)
    }

  def hoist[N[_]](mn: M ~> N): StepT[N, F, S] = new StepT[N, F, S] {
    def apply[K[_], A](f: F[K, A]): WriterStateT[N, Lst[K[Unit]], S[K], A] = {
      val ws = self(f)
      WriterStateT(s => mn(ws(s)))
    }
  }

  final def :&&:[G[_[_], _], T[_[_]]](
    that: StepT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StepT[M, CoproductK[G, F, ?[_], ?], (T :**: S)#Out] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K[_]] = (T :**: S)#Out[K]
    new StepT[M, H, U] {
      override def apply[K[_], A](h: H[K, A]): WriterStateT[M, Lst[K[Unit]], U[K], A] =
        h.run match {
          case -\/(g) => that(g).zoomOut
          case \/-(f) => self(f).zoomOut
        }
    }
  }

  final def :&&:[G[_[_], _], T[_[_]]](
    ig: StateInterpreterT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], (T :**: S)#Out] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K[_]] = (T :**: S)#Out[K]

    new StateInterpreterT[M, H, U] {
      def step: StepT[M, H, U] = ig.step :&&: self
      def uncons: Uncons[U] =
        ig.uncons.zoomOut[U]
    }
  }

  def ::[G[_[_], _]](that: StepT[M, G, S]): StepT[M, CoproductK[G, F, ?[_], ?], S] =
    new StepT[M, CoproductK[G, F, ?[_], ?], S] {
      def apply[K[_], A](ca: CoproductK[G, F, K, A]): WriterStateT[M, Lst[K[Unit]], S[K], A] =
        WriterStateT(s => ca.run.fold(that(_), self(_))(s))
    }

  def :>>:[G[_[_], _]](ig: G ≈>> M)(implicit M: Monad[M]): StepT[M, CoproductK[G, F, ?[_], ?], S] =
    StepT.lift[M, G, S](ig) :: self

}

object StepT {
  import scala.language.implicitConversions

  def lift[M[_]: Monad, F[_[_], _], S[_[_]]](fm: F ≈>> M): StepT[M, F, S] =
    new StepT[M, F, S] {
      override def apply[K[_], A](f: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K], A] =
        WriterStateT.monadTrans[Lst[K[Unit]], S[K]].liftM(fm(f))
    }

  final case class StepTOps[M[_], F[_[_], _], S[_[_]]](self: StepT[M, F, S]) extends AnyVal {

    def :&:[G[_[_], _], T[_[_]]](
      that: StepT[M, G, T]
    )(implicit
      M: Functor[M]
    ): StepT[M, CoproductK[G, F, ?[_], ?], (T :**: S)#Out] = {
      type H[K[_], A] = CoproductK[G, F, K, A]
      type U[K[_]] = KPair[T, S, K]

      new StepT[M, H, U] {
        override def apply[K[_], A](h: H[K, A]): WriterStateT[M, Lst[K[Unit]], U[K], A] =
          h.run match {
            case -\/(g) => that(g).zoomOut
            case \/-(f) => self(f).zoomOut
          }
      }
    }

    def :&:[G[_[_], _], T[_[_]]](
      ig: StateInterpreterT[M, G, T]
    )(implicit
      M: Functor[M]
    ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], (T :**: S)#Out] = {
      type H[K[_], A] = CoproductK[G, F, K, A]
      type U[K[_]] = KPair[T, S, K]

      new StateInterpreterT[M, H, U] {
        def step: StepT[M, H, U] = ig.step :&: self
        def uncons: Uncons[U] =
          ig.uncons.zoomOut[U]
      }
    }

  }

  implicit def toOps[M[_], F[_[_], _], S[_[_]]](self: StepT[M, F, S]): StepTOps[M, F, S] =
    StepTOps(self)
}
