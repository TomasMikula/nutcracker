package nutcracker.util

import scalaz.{Coproduct, Functor, Monad, ~>, NonEmptyList => Nel}
import scalaz.syntax.functor._

abstract class StepT[M[_], K[_], F[_], S] extends (F ~> WriterStateT[M, Lst[K[Unit]], S, ?]) { self =>

  def apply[A](f: F[A]): WriterStateT[M, Lst[K[Unit]], S, A]

  def hoist[N[_]](mn: M ~> N): StepT[N, K, F, S] = new StepT[N, K, F, S] {
    def apply[A](f: F[A]): WriterStateT[N, Lst[K[Unit]], S, A] = {
      val ws = self(f)
      WriterStateT(s => mn(ws(s)))
    }
  }

  def :+:[G[_]](that: StepT[M, K, G, S]): StepT[M, K, Coproduct[G, F, ?], S] = {
    type H[A] = Coproduct[G, F, A]

    new StepT[M, K, H, S] {
      def apply[A](ca: H[A]): WriterStateT[M, Lst[K[Unit]], S, A] =
        WriterStateT(s => ca.run.fold(that(_), self(_))(s))
    }
  }

  def :+:[G[_]](that: StateInterpreterT[M, K, G, S]): StateInterpreterT[M, K, Coproduct[G, F, ?], S] = {
    type H[A] = Coproduct[G, F, A]

    new StateInterpreterT[M, K, H, S] {
      def step: StepT[M, K, H, S] = that.step :+: self
      def uncons: Uncons[K, S] = that.uncons
    }
  }

  def :>>:[G[_]](ig: G ~> M)(implicit M: Monad[M]): StepT[M, K, Coproduct[G, F, ?], S] =
    StepT.lift[M, K, G, S](ig) :+: self


  def inHead(implicit M: Functor[M]): StepT[M, K, F, Nel[S]] = new StepT[M, K, F, Nel[S]] {
    def apply[A](f: F[A]) =
      WriterStateT((s: Nel[S]) => self(f).apply(s.head) map { case (ks, h, a) => (ks, Nel.nel(h, s.tail), a) })
  }
}

object StepT {

  def lift[M[_]: Monad, K[_], F[_], S](fm: F ~> M): StepT[M, K, F, S] =
    new StepT[M, K, F, S] {
      override def apply[A](f: F[A]): WriterStateT[M, Lst[K[Unit]], S, A] =
        WriterStateT.monadTrans[Lst[K[Unit]], S].liftM(fm(f))
    }
}
