package nutcracker

import nutcracker.util.{FreeK, InjectK, Step}
import scalaz.Id.Id
import scalaz.~>

trait BranchingModule extends Module {
  type Var[A]
  type Val[A]
  type Lang[K[_], A]
  type State[K[_]]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], Var, Val]
  ): BranchingPropagation[FreeK[F, ?], Var, Val]

  def empty[K[_]]: State[K]
  def interpreter: Step[Lang, State]
  def assess[K[_]](s: State[K])(fetch: Var ~> Id)(implicit K: Propagation[K, Var, Val]): Assessment[List[K[Unit]]]
}

trait PersistentBranchingModule extends BranchingModule with PersistentStateModule { self =>
  override def stashable: BranchingModule with StashModule {
    type Var[A] = self.Var[A]
    type Val[A] = self.Val[A]
    type Lang[K[_], A] = self.Lang[K, A]
  }
}

object PersistentBranchingModule {
  type Aux[Var0[_], Val0[_], Lang0[_[_], _], State0[_[_]]] = PersistentBranchingModule {
    type Var[A] = Var0[A]
    type Val[A] = Val0[A]
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

class BranchingListModule[Var0[_], Val0[_], Lang[_[_], _], State[_[_]]](base: PersistentBranchingModule.Aux[Var0, Val0, Lang, State])
extends ListModule[Lang, State](base) with BranchingModule {
  type Var[A] = Var0[A]
  type Val[A] = Val0[A]

  def freeBranchingPropagation[F[_[_], _]](implicit
    i: InjectK[Lang, F],
    P: Propagation[FreeK[F, ?], Var, Val]
  ): BranchingPropagation[FreeK[F, ?], Var, Val] =
    base.freeBranchingPropagation[F]

  def interpreter: Step[Lang, State] = base.interpreter.inHead
  def assess[K[_]](s: State[K])(fetch: Var ~> Id)(implicit K: Propagation[K, Var, Val]): Assessment[List[K[Unit]]] =
    base.assess[K](s.head)(fetch)
}