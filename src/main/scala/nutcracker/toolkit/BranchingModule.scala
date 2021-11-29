package nutcracker.toolkit

import nutcracker.util.{FreeK, Inject, StateInterpreter}
import nutcracker.{Assessment, BranchingPropagation, Propagation}
import scalaz.Id.Id
import scalaz.{Lens, ~>}

trait BranchingModule extends Module {
  type VarK[K[_], A]
  type ValK[K[_], A]
  type Lang[K[_], A]
  type StateK[K[_]]

  implicit def freeBranchingPropagation[F[_[_], _]](implicit
    i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]],
    P: Propagation[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]]
  ): BranchingPropagation[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]]

  def emptyK[K[_]]: StateK[K]
  def stepInterpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S]
  def assess[K[_]](s: StateK[K])(fetch: VarK[K, *] ~> Id)(implicit K: Propagation[K, VarK[K, *], ValK[K, *]]): Assessment[List[K[Unit]]]
}

object BranchingModule {
  type Aux0[Var0[_[_], _], Val0[_[_], _]] = BranchingModule {
    type VarK[K[_], A] = Var0[K, A]
    type ValK[K[_], A] = Val0[K, A]
  }

  type AuxL[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _]] = Aux0[Var0, Val0] {
    type Lang[K[_], A] = Lang0[K, A]
  }

  type Aux[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]] = AuxL[Var0, Val0, Lang0] {
    type StateK[K[_]] = State0[K]
  }
}

trait PersistentBranchingModule extends BranchingModule with PersistentStateModule { self =>
  override def stashable: BranchingModule.AuxL[self.VarK, self.ValK, self.Lang] with StashModule
}

object PersistentBranchingModule {
  type Aux0[Var0[_[_], _], Val0[_[_], _]] = PersistentBranchingModule {
    type VarK[K[_], A] = Var0[K, A]
    type ValK[K[_], A] = Val0[K, A]
  }

  type Aux[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]] =
    BranchingModule.Aux[Var0, Val0, Lang0, State0] with PersistentBranchingModule

  def instance[Var0[_[_], _], Val0[_[_], _]]: PersistentBranchingModule { type VarK[K[_], A] = Var0[K, A]; type ValK[K[_], A] = Val0[K, A] } =
    new BranchingModuleImpl[Var0, Val0]
}

class BranchingListModule[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]](base: PersistentBranchingModule.Aux[Var0, Val0, Lang0, State0])
extends ListModule[Lang0, State0](base) with BranchingModule {
  type VarK[K[_], A] = Var0[K, A]
  type ValK[K[_], A] = Val0[K, A]
  override type Lang[K[_], A] = Lang0[K, A]

  override def freeBranchingPropagation[F[_[_], _]](implicit
    i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]],
    P: Propagation[FreeK[F, *], Var0[FreeK[F, *], *], Val0[FreeK[F, *], *]]
  ): BranchingPropagation[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]] =
    base.freeBranchingPropagation[F](i, P)

  override def stepInterpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] =
    base.stepInterpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))

  override def assess[K[_]](s: StateK[K])(fetch: VarK[K, *] ~> Id)(implicit K: Propagation[K, VarK[K, *], ValK[K, *]]): Assessment[List[K[Unit]]] =
    base.assess[K](s.head)(fetch)
}