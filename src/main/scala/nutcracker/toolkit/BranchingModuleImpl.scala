package nutcracker.toolkit

import nutcracker.ops._
import nutcracker.util.{FreeK, Inject, Lst, Step, WriterState}
import nutcracker.util.ops._
import nutcracker.{Assessment, BranchingPropagation, Propagation, Splittable}
import scalaz.Id.Id
import scalaz.{Lens, ~>}

private[nutcracker] class BranchingModuleImpl[Var0[_[_], _], Val0[_[_], _]] extends PersistentBranchingModule {
  type VarK[K[_], A] = Var0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  type Lang[K[_], A] = BranchLang[Var0[K, ?], K, A]
  type StateK[K[_]] = BranchStore[Var0[K, ?], K]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]],
    P: Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
  ): BranchingPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    new BranchingPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] {
      override val propagation: Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = P

      def newVar[A](a: A)(implicit ev: Splittable[A]): FreeK[F, VarK[FreeK[F, ?], A]] =
        if(ev.isUnresolved(a))
          for {
            ref <- propagation.newCell[A](a)
            _ <- BranchLang.trackF[VarK[FreeK[F, ?], ?], F, A](ref)
            _ <- ref.observe.threshold(a =>
              if(ev.isUnresolved(a)) None
              else Some(BranchLang.untrackF[VarK[FreeK[F, ?], ?], F, A](ref))
            )
          } yield ref
        else
          propagation.newCell[A](a)
    }

  def emptyK[K[_]]: StateK[K] = BranchStore()

  def interpreter[K[_], S](implicit l: Lens[S, StateK[K]]): Step[K, Lang[K, ?], S] = new Step[K, Lang[K, ?], S] {

    def apply[A](f: BranchLang[VarK[K, ?], K, A]): WriterState[Lst[K[Unit]], S, A] = WriterState(s0 => {
      val s = l.get(s0)
      f.fold(
        caseTrack = t => t match { case t1 => (Lst.empty, s0 set s.addVar(t1.ref, t1.ev), t1.wit(())) },
        caseUntrack = u => (Lst.empty, s0 set s.removeVar(u.ref), u.wit(()))
      )
    })
  }

  def assess[K[_]](s: StateK[K])(fetch: VarK[K, ?] ~> Id)(implicit K: Propagation[K, VarK[K, ?], ValK[K, ?]]): Assessment[List[K[Unit]]] =
    s.split(fetch)

  def stashable = new BranchingListModule[VarK, ValK, Lang, StateK](this)
}
