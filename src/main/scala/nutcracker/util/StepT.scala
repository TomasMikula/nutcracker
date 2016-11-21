package nutcracker.util

import scala.language.higherKinds
import scalaz.{Functor, Lens, Monad, -\/, \/-, ~>}
import nutcracker.util.KList._

abstract class StepT[M[_], F[_[_], _], S[_]] { self =>

  def apply[K[_], A](f: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A]

  def papply[K[_]]: F[K, ?] ~> WriterStateT[M, Lst[K[Unit]], S[K[Unit]], ?] =
    new (F[K, ?] ~> WriterStateT[M, Lst[K[Unit]], S[K[Unit]], ?]) {
      def apply[A](fa: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A] = self(fa)
    }

  def hoist[N[_]](mn: M ~> N): StepT[N, F, S] = new StepT[N, F, S] {
    def apply[K[_], A](f: F[K, A]): WriterStateT[N, Lst[K[Unit]], S[K[Unit]], A] = {
      val ws = self(f)
      WriterStateT(s => mn(ws(s)))
    }
  }

  final def :&&:[G[_[_], _], T[_]](
    that: StepT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StepT[M, CoproductK[G, F, ?[_], ?], Cons[T, Just[S, ?], ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K] = Cons[T, Just[S, ?], K]
    new StepT[M, H, U] {
      override def apply[K[_], A](h: H[K, A]): WriterStateT[M, Lst[K[Unit]], U[K[Unit]], A] =
        h.run match {
          case -\/(g) => that(g).zoomOut(implicitly[Lens[U[K[Unit]], T[K[Unit]]]])
          case \/-(f) => self(f).zoomOut(implicitly[Lens[U[K[Unit]], S[K[Unit]]]])
        }
    }
  }

  final def :&&:[G[_[_], _], T[_]](
    ig: StateInterpreterT[M, G, T]
  )(implicit
    M: Functor[M]
  ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], Cons[T, Just[S, ?], ?]] = {
    type H[K[_], A] = CoproductK[G, F, K, A]
    type U[K] = Cons[T, Just[S, ?], K]

    new StateInterpreterT[M, H, U] {
      def step: StepT[M, H, U] = ig.step :&&: self
      def uncons: Uncons[U] =
        ig.uncons.zoomOut[U](KList.headLenzK[T, Just[S, ?]])
    }
  }

  def ::[G[_[_], _]](that: StepT[M, G, S]): StepT[M, CoproductK[G, F, ?[_], ?], S] =
    new StepT[M, CoproductK[G, F, ?[_], ?], S] {
      def apply[K[_], A](ca: CoproductK[G, F, K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A] =
        WriterStateT(s => ca.run.fold(that(_), self(_))(s))
    }

  def :>>:[G[_[_], _]](ig: G ≈>> M)(implicit M: Monad[M]): StepT[M, CoproductK[G, F, ?[_], ?], S] =
    StepT.lift[M, G, S](ig) :: self

}

object StepT {
  import scala.language.implicitConversions

  def lift[M[_]: Monad, F[_[_], _], S[_]](fm: F ≈>> M): StepT[M, F, S] =
    new StepT[M, F, S] {
      override def apply[K[_], A](f: F[K, A]): WriterStateT[M, Lst[K[Unit]], S[K[Unit]], A] =
        WriterStateT.monadTrans[Lst[K[Unit]], S[K[Unit]]].liftM(fm(f))
    }

  final case class StepTOps[M[_], F[_[_], _], S[_] <: KList[_]](self: StepT[M, F, S]) extends AnyVal {

    def :&:[G[_[_], _], T[_]](
      that: StepT[M, G, T]
    )(implicit
      M: Functor[M]
    ): StepT[M, CoproductK[G, F, ?[_], ?], Cons[T, S, ?]] = {
      type H[K[_], A] = CoproductK[G, F, K, A]
      type U[K] = Cons[T, S, K]

      new StepT[M, H, U] {
        override def apply[K[_], A](h: H[K, A]): WriterStateT[M, Lst[K[Unit]], U[K[Unit]], A] =
          h.run match {
            case -\/(g) => that(g).zoomOut(implicitly[Lens[U[K[Unit]], T[K[Unit]]]])
            case \/-(f) => self(f).zoomOut(implicitly[Lens[U[K[Unit]], S[K[Unit]]]])
          }
      }
    }

    def :&:[G[_[_], _], T[_]](
      ig: StateInterpreterT[M, G, T]
    )(implicit
      M: Functor[M]
    ): StateInterpreterT[M, CoproductK[G, F, ?[_], ?], Cons[T, S, ?]] = {
      type H[K[_], A] = CoproductK[G, F, K, A]
      type U[K] = Cons[T, S, K]

      new StateInterpreterT[M, H, U] {
        def step: StepT[M, H, U] = ig.step :&: self
        def uncons: Uncons[U] =
          ig.uncons.zoomOut[U](implicitly[`Forall{* -> *}`[λ[K => Lens[U[K], T[K]]]]])
      }
    }

  }

  implicit def toOps[M[_], F[_[_], _], S[_] <: KList[_]](self: StepT[M, F, S]): StepTOps[M, F, S] =
    StepTOps(self)
}
