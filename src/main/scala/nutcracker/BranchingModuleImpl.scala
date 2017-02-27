package nutcracker

import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import scalaz.Id.Id
import scalaz.~>

private[nutcracker] class BranchingModuleImpl[Ref0[_]] extends PersistentBranchingModule {
  type Ref[A] = Ref0[A]
  type Lang[K[_], A] = BranchLang[Ref, K, A]
  type State[K[_]] = BranchStore[Ref, K]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], Ref]
  ): BranchingPropagation[FreeK[F, ?], Ref] =
    new BranchingPropagation[FreeK[F, ?], Ref] {
      val propagation: Propagation[FreeK[F, ?], Ref] = P

      def newVar[A](a: A)(implicit ev: Splittable[A]): FreeK[F, Ref[A]] =
        if(ev.isUnresolved(a))
          for {
            ref <- propagation.newCell[A](a)
            _ <- BranchLang.trackF(ref)
            _ <- propagation.observe(ref).threshold(a =>
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
    def apply[K[_], A](f: BranchLang[Ref, K, A]): WriterState[Lst[K[Unit]], State[K], A] = f match {
      case Track(ref, ev) => WriterState(s => (Lst.empty, s.addVar(ref, ev), ()))
      case Untrack(ref) => WriterState(s => (Lst.empty, s.removeVar(ref), ()))
    }
  }

  def assess[K[_]](s: State[K])(fetch: Ref ~> Id)(implicit K: Propagation[K, Ref]): Assessment[List[K[Unit]]] =
    s.split(fetch)

  def stashable = new BranchingListModule[Ref, Lang, State](this)
}
