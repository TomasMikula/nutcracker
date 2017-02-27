package nutcracker

import nutcracker.util.{FreeK, InjectK, Step}
import scalaz.Id.Id
import scalaz.~>

trait BranchingModule extends Module {
  type Ref[A]
  type Lang[K[_], A]
  type State[K[_]]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], Ref]
  ): BranchingPropagation[FreeK[F, ?], Ref]

  def empty[K[_]]: State[K]
  def interpreter: Step[Lang, State]
  def assess[K[_]](s: State[K])(fetch: Ref ~> Id)(implicit K: Propagation[K, Ref]): Assessment[List[K[Unit]]]
}

trait PersistentBranchingModule extends BranchingModule with PersistentStateModule { self =>
  override def stashable: BranchingModule with StashModule {
    type Ref[A] = self.Ref[A]
    type Lang[K[_], A] = self.Lang[K, A]
  }
}

object PersistentBranchingModule {
  type Aux[Ref0[_], Lang0[_[_], _], State0[_[_]]] = PersistentBranchingModule {
    type Ref[A] = Ref0[A]
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

class BranchingListModule[Ref0[_], Lang[_[_], _], State[_[_]]](base: PersistentBranchingModule.Aux[Ref0, Lang, State])
extends ListModule[Lang, State](base) with BranchingModule {
  type Ref[A] = Ref0[A]

  def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], Ref]
  ): BranchingPropagation[FreeK[F, ?], Ref] =
    base.freeBranchingPropagation[F]

  def interpreter: Step[Lang, State] = base.interpreter.inHead
  def assess[K[_]](s: State[K])(fetch: Ref ~> Id)(implicit K: Propagation[K, Ref]): Assessment[List[K[Unit]]] =
    base.assess[K](s.head)(fetch)
}