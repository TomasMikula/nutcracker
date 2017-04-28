package nutcracker

import nutcracker.ops._
import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import scalaz.Id.Id
import scalaz.{Monad, ~>}

private[nutcracker] class BranchingModuleImpl[Var0[_], Val0[_]] extends PersistentBranchingModule {
  type Var[A] = Var0[A]
  type Val[A] = Val0[A]
  type Lang[K[_], A] = BranchLang[Var, K, A]
  type State[K[_]] = BranchStore[Var, K]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], Var, Val]
  ): BranchingPropagation[FreeK[F, ?], Var, Val] =
    new BranchingPropagation[FreeK[F, ?], Var, Val] {
      override val propagation: Propagation[FreeK[F, ?], Var, Val] = P

      def newVar[A](a: A)(implicit ev: Splittable[A]): FreeK[F, Var[A]] =
        if(ev.isUnresolved(a))
          for {
            ref <- propagation.newCell[A](a)
            _ <- BranchLang.trackF(ref)(ev, i[FreeK[F, ?]])
            _ <- ref.observe.threshold(a =>
              if(ev.isUnresolved(a)) None
              else Some(BranchLang.untrackF(ref))
            )
          } yield ref
        else
          propagation.newCell[A](a)
    }

  def empty[K[_]]: State[K] = BranchStore()

  def interpreter: Step[Lang, State] = new Step[Lang, State] {
    import BranchLang._
    def apply[K[_]: Monad, A](f: BranchLang[Var, K, A]): WriterState[Lst[K[Unit]], State[K], A] = f match {
      case Track(ref, ev) => WriterState(s => (Lst.empty, s.addVar(ref, ev), ()))
      case Untrack(ref) => WriterState(s => (Lst.empty, s.removeVar(ref), ()))
    }
  }

  def assess[K[_]](s: State[K])(fetch: Var ~> Id)(implicit K: Propagation[K, Var, Val]): Assessment[List[K[Unit]]] =
    s.split(fetch)

  def stashable = new BranchingListModule[Var, Val, Lang, State](this)
}
