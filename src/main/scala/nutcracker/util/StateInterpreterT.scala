package nutcracker.util

import nutcracker.util.free.Free
import scalaz.{BindRec, Coproduct, Functor, Lens, Monad, StateT, Store, \/, ~>, NonEmptyList => Nel}
import scalaz.Id._
import scalaz.syntax.applicative._
import scalaz.syntax.either._

trait StateInterpreterT[M[_], K[_], F[_], S] { self =>
  def step: StepT[M, K, F, S]
  def uncons: Uncons[K, S]

  final def :+:[G[_]](that: StateInterpreterT[M, K, G, S]): StateInterpreterT[M, K, Coproduct[G, F, ?], S] = {
    type H[A] = Coproduct[G, F, A]

    new StateInterpreterT[M, K, H, S] {
      def step: StepT[M, K, H, S] = that.step :+: self.step
      def uncons: Uncons[K, S] = that.uncons orElse self.uncons
    }
  }

  final def :+:[G[_]](that: StepT[M, K, G, S]): StateInterpreterT[M, K, Coproduct[G, F, ?], S] = {
    type H[A] = Coproduct[G, F, A]

    new StateInterpreterT[M, K, H, S] {
      def step: StepT[M, K, H, S] = that :+: self.step
      def uncons: Uncons[K, S] = self.uncons
    }
  }

  final def :>>:[G[_]](
    ig: G ~> M
  )(implicit
    M: Monad[M]
  ): StateInterpreterT[M, K, Coproduct[G, F, ?], S] =
    new StateInterpreterT[M, K, Coproduct[G, F, ?], S] {
      def step = ig :>>: self.step
      def uncons = self.uncons
    }

  def hoist[N[_]](mn: M ~> N)(implicit M: Monad[M], N: Monad[N]): StateInterpreterT[N, K, F, S] =
    new StateInterpreterT[N, K, F, S] {
      def step: StepT[N, K, F, S] = self.step.hoist(mn)
      def uncons: Uncons[K, S] = self.uncons
    }

  def hoistId[N[_]](implicit N: Monad[N], ev: this.type <:< StateInterpreterT[Id, K, F, S]): StateInterpreterT[N, K, F, S] =
    ev(this).hoist(idToM[N])

  def freeInstance(f: K[Unit] => Free[F, Unit])(implicit M0: Monad[M], M1: BindRec[M]): Free[F, ?] ~> StateT[M, S, ?] =
    StateInterpreterT.freeInstance(step, uncons)(f)

  def inHead(implicit M: Functor[M]): StateInterpreterT[M, K, F, Nel[S]] = new StateInterpreterT[M, K, F, Nel[S]] {
    val step = self.step.inHead

    val uncons = self.uncons.zoomOut[Nel[S]](Lens[Nel[S], S](l => Store(s => Nel.nel(s, l.tail), l.head)))
  }
}

object StateInterpreterT {

  def freeInstance[M[_], K[_], F[_], S](
    step: StepT[M, K, F, S],
    uncons: Uncons[K, S]
  )(
    f: K[Unit] => Free[F, Unit]
  )(implicit
    M0: Monad[M],
    M1: BindRec[M]
  ): Free[F, ?] ~> StateT[M, S, ?] = {

    def runUntilClean[A](p: Free[F, A])(s: S): M[(S, A)] = {
      M0.bind(runToCompletion(p)(s)){ case (s1, a) => M0.map(runUntilClean1(s1)) {(_, a)} }
    }

    def runUntilClean1(s: S): M[S] =
      M1.tailrecM(s)(s => uncons(s) match {
        case None => s.right.point[M]
        case Some((s1, ku)) => M0.map(runToCompletionU(ku)(s1)){ _.left }
      })

    def runToCompletion[A](p: Free[F, A])(s: S): M[(S, A)] =
      M0.bind(p.foldMap(step).apply(s)) {
        case (ku, s1, a) => M0.map(runToCompletionU(ku)(s1)) { (_, a) }
      }

    def runToCompletionU(ps: Lst[K[Unit]])(s: S): M[S] = {
      def go(a: (Lst[K[Unit]], S)): M[(Lst[K[Unit]], S) \/ S] = {
        val (l, s) = a
        l.uncons match {
          case None => s.right.point[M]
          case Some((k, ks)) => M0.map(f(k).foldMap(step).apply(s)) { case (ks1, s1, ()) => (ks1 ++ ks, s1).left }
        }
      }
      M1.tailrecM((ps, s))(go)
    }

    λ[Free[F, ?] ~> StateT[M, S, ?]](fa => StateT(runUntilClean(fa)))
  }
}
