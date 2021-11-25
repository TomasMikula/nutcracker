package nutcracker.util

import nutcracker.util.free.Free
import scalaz.{-\/, Bind, BindRec, Coproduct, \/-, ~>}

abstract class StateInterpreter[K[_], F[_], S] { self =>

  def apply[M[_], W, A](fa: F[A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[F, K], K: Bind[K]): M[A]

  def apply[M[_], W](implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[F, K], K: Bind[K]): F ~> M =
    new (F ~> M) {
      override def apply[A](fa: F[A]): M[A] = self(fa)
    }

  def free[M[_], W](implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], M1: BindRec[M], inj: Inject[F, K], K: Bind[K]): Free[F, *] ~> M =
    new (Free[F, *] ~> M) {
      override def apply[A](fa: Free[F, A]): M[A] =
        fa.foldMap(self[M, W])
    }

  def :+:[G[_]](that: StateInterpreter[K, G, S]): StateInterpreter[K, Coproduct[G, F, *], S] = {
    type H[A] = Coproduct[G, F, A]

    new StateInterpreter[K, H, S] {
      def apply[M[_], W, A](ha: H[A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[H, K], K: Bind[K]): M[A] =
        ha.run match {
          case -\/(ga) => that(ga)(M, W, inj.compose(Inject.injectLeft),  K)
          case \/-(fa) => self(fa)(M, W, inj.compose(Inject.injectRight), K)
        }
    }
  }
}