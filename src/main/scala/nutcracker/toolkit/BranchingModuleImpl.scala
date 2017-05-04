package nutcracker.toolkit

import nutcracker.ops._
import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import nutcracker.{Assessment, BranchingPropagation, Propagation, Splittable}
import scalaz.Id.Id
import scalaz.{Monad, ~>}

private[nutcracker] class BranchingModuleImpl[Var0[_[_], _], Val0[_[_], _]] extends PersistentBranchingModule {
  type VarK[K[_], A] = Var0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  type Lang[K[_], A] = BranchLang[Var0[K, ?], K, A]
  type StateK[K[_]] = BranchStore[Var0[K, ?], K]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
  ): BranchingPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    new BranchingPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] {
      override val propagation: Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = P

      def newVar[A](a: A)(implicit ev: Splittable[A]): FreeK[F, VarK[FreeK[F, ?], A]] =
        if(ev.isUnresolved(a))
          for {
            ref <- propagation.newCell[A](a)
            _ <- BranchLang.trackF[VarK[FreeK[F, ?], ?], F, A](ref)(ev, i[FreeK[F, ?]])
            _ <- ref.observe.threshold(a =>
              if(ev.isUnresolved(a)) None
              else Some(BranchLang.untrackF[VarK[FreeK[F, ?], ?], F, A](ref)(i[FreeK[F, ?]]))
            )
          } yield ref
        else
          propagation.newCell[A](a)
    }

  def emptyK[K[_]]: StateK[K] = BranchStore()

  def interpreter: Step[Lang, StateK] = new Step[Lang, StateK] {
    import BranchLang._

    def apply[K[_]: Monad, A](f: BranchLang[VarK[K, ?], K, A]): WriterState[Lst[K[Unit]], StateK[K], A] =
      go[VarK[K, ?], K, A](f)

    // https://github.com/scala/bug/issues/10292
    private def go[Ref[_], K[_]: Monad, A](f: BranchLang[Ref, K, A]): WriterState[Lst[K[Unit]], BranchStore[Ref, K], A] = f match {
      case Track(ref, ev) => WriterState(s => (Lst.empty, s.addVar(ref, ev), ()))
      case Untrack(ref) => WriterState(s => (Lst.empty, s.removeVar(ref), ()))
    }
  }

  def assess[K[_]](s: StateK[K])(fetch: VarK[K, ?] ~> Id)(implicit K: Propagation[K, VarK[K, ?], ValK[K, ?]]): Assessment[List[K[Unit]]] =
    s.split(fetch)

  def stashable = new BranchingListModule[VarK, ValK, Lang, StateK](this)
}
