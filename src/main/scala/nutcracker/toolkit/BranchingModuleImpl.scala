package nutcracker.toolkit

import nutcracker.ops.Ops._
import nutcracker.util.{FreeK, Inject, Lst, MonadTellState, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops.Ops._
import nutcracker.{Assessment, BranchingPropagation, Propagation, Splittable, Subscription}
import scalaz.Id.Id
import scalaz.{Bind, Lens, ~>}
import scalaz.syntax.functor._

private[nutcracker] class BranchingModuleImpl[Var0[_[_], _], Val0[_[_], _]] extends PersistentBranchingModule {
  type VarK[K[_], A] = Var0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  type Lang[K[_], A] = BranchLang[Var0[K, *], K, A]
  type StateK[K[_]] = BranchStore[Var0[K, *], K]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]],
    P: Propagation.Aux[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]]
  ): BranchingPropagation[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]] =
    new BranchingPropagation[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]] {
      override val propagation: Propagation.Aux[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]] = P

      def newVar[A](a: A)(implicit ev: Splittable[A]): FreeK[F, VarK[FreeK[F, *], A]] =
        for {
          ref <- propagation.newCell[A](a)
          _ <- observeVar(ref)
        } yield ref

      private def observeVar[A](ref: VarK[FreeK[F, *], A])(implicit ev: Splittable[A]): FreeK[F, Subscription[FreeK[F, *]]] =
        ref.observe.by(a =>
          if(ev.isUnresolved(a)) P.reconsider(addUnresolvedF(ref) as P.sleep(unresolvedObserver(ref)))
          else if(!ev.isFailed(a)) P.sleep(resolvedObserver(ref))
          else P.fire(addFailedF(ref))
        )

      private def unresolvedObserver[A](ref: VarK[FreeK[F, *], A])(implicit ev: Splittable[A]): (A, ev.Delta) => P.Trigger[A, ev.Delta] =
        P.thresholdTransition1(a =>
          if(ev.isUnresolved(a)) None
          else if(!ev.isFailed(a)) Some(P.reconsider(rmUnresolvedF(ref) as P.sleep(resolvedObserver(ref))))
          else Some(P.fire(rmUnresolvedF(ref) >> addFailedF(ref)))
        )

      private def resolvedObserver[A](ref: VarK[FreeK[F, *], A])(implicit ev: Splittable[A]): (A, ev.Delta) => P.Trigger[A, ev.Delta] =
        P.threshold1(a => if(ev.isFailed(a)) Some(addFailedF(ref)) else None)

      private def addUnresolvedF[A](ref: VarK[FreeK[F, *], A])(implicit ev: Splittable[A]): FreeK[F, Unit] =
        BranchLang.addUnresolvedF[VarK[FreeK[F, *], *], F, A](ref)

      private def rmUnresolvedF[A](ref: VarK[FreeK[F, *], A]): FreeK[F, Unit] =
        BranchLang.rmUnresolvedF[VarK[FreeK[F, *], *], F, A](ref)

      private def addFailedF[A](ref: VarK[FreeK[F, *], A]): FreeK[F, Unit] =
        BranchLang.addFailedF[VarK[FreeK[F, *], *], F, A](ref)
    }

  def emptyK[K[_]]: StateK[K] = BranchStore()

  def stepInterpreter[K[_], S](implicit l: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] = new StateInterpreter[K, Lang[K, *], S] {

    def apply[M[_], W, A](fa: BranchLang[Var0[K, *], K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[BranchLang[Var0[K, *], K, *], K], K: Bind[K]): M[A] =
      M.writerState[A](s0 => {
        val s = l.get(s0)
        fa.fold(
          caseAddUnresolved = t => t match { case t1 => (W.zero, s0 set s.addUnresolved(t1.ref, t1.ev), t1.wit(())) },
          caseRmUnresolved = u => (W.zero, s0 set s.removeUnresolved(u.ref), u.wit(())),
          caseAddFailed = i => (W.zero, s0 set s.addFailed(i.ref), i.wit(()))
        )
      })
  }

  def assess[K[_]](s: StateK[K])(fetch: VarK[K, *] ~> Id)(implicit K: Propagation.Aux[K, VarK[K, *], ValK[K, *]]): Assessment[List[K[Unit]]] =
    if(s.hasFailedVars) Assessment.Failed
    else s.split(fetch)

  def stashable: BranchingListModule[VarK, ValK, Lang, StateK] =
    new BranchingListModule[VarK, ValK, Lang, StateK](this)
}
