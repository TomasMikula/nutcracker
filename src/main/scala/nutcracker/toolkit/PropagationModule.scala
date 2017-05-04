package nutcracker.toolkit

import nutcracker.util.{FreeK, HOrderK, InjectK, ShowK, StateInterpreter}
import nutcracker.{OnDemandPropagation, Propagation}

trait PropagationModule extends Module {
  type VarK[K[_], A]
  type ValK[K[_], A]

  implicit def varOrderK[K[_]]: HOrderK[VarK[K, ?]]
  implicit def varShowK[K[_]]: ShowK[VarK[K, ?]]
  implicit def valOrderK[K[_]]: HOrderK[ValK[K, ?]]
  implicit def valShowK[K[_]]: ShowK[ValK[K, ?]]
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]

  def interpreter: StateInterpreter[Lang, StateK]
  def isConsistent[K[_]](s: StateK[K]): Boolean
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A
  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A]
}

object PropagationModule {
  type AuxL[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _]] = Module.AuxL[Lang0] with PropagationModule {
    type VarK[K[_], A] = Var0[K, A]
    type ValK[K[_], A] = Val0[K, A]
  }

  type Aux[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]] = AuxL[Var0, Val0, Lang0] {
    type StateK[K[_]] = State0[K]
  }
}

trait PersistentPropagationModule extends PropagationModule with PersistentStateModule { self =>
  override def stashable: StashPropagationModule.AuxL[self.VarK, self.ValK, self.Lang]
}

object PersistentPropagationModule {
  type Aux[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]] = PropagationModule.Aux[Var0, Val0, Lang0, State0] with PersistentStateModule
}

trait StashPropagationModule extends PropagationModule with StashModule

object StashPropagationModule {
  type AuxL[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _]] = PropagationModule.AuxL[Var0, Val0, Lang0] with StashPropagationModule
}

class PropagationListModule[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]](base: PersistentPropagationModule.Aux[Var0, Val0, Lang0, State0])
extends ListModule[Lang0, State0](base) with StashPropagationModule {
  type VarK[K[_], A] = Var0[K, A]
  type ValK[K[_], A] = Val0[K, A]

  override implicit def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = base.varOrderK
  override implicit def varShowK[K[_]]: ShowK[VarK[K, ?]] = base.varShowK
  override implicit def valOrderK[K[_]]: HOrderK[ValK[K, ?]] = base.valOrderK
  override implicit def valShowK[K[_]]: ShowK[ValK[K, ?]] = base.valShowK

  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = base.freePropagation

  def interpreter: StateInterpreter[Lang, StateK] = base.interpreter.inHead
  def isConsistent[K[_]](s: StateK[K]): Boolean = base.isConsistent[K](s.head)
  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] = base.fetchK[K, A](ref, s.head)
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A = base.fetchK[K, A](ref, s.head)
}

trait OnDemandPropagationModule extends PropagationModule {
  implicit override def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): OnDemandPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]]
}

object OnDemandPropagationModule {
  type AuxL[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _]] = PropagationModule.AuxL[Var0, Val0, Lang0] with OnDemandPropagationModule
}

trait PersistentOnDemandPropagationModule extends OnDemandPropagationModule with PersistentPropagationModule { self =>
  override def stashable: StashOnDemandPropagationModule.AuxL[self.VarK, self.ValK, self.Lang]
}

object PersistentOnDemandPropagationModule {
  type Aux[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]] = PersistentOnDemandPropagationModule with PersistentPropagationModule.Aux[Var0, Val0, Lang0, State0]
}

trait StashOnDemandPropagationModule extends OnDemandPropagationModule with StashPropagationModule

object StashOnDemandPropagationModule {
  type AuxL[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _]] = OnDemandPropagationModule.AuxL[Var0, Val0, Lang0] with StashPropagationModule
}

class OnDemandPropagationListModule[Var0[_[_], _], Val0[_[_], _], Lang0[_[_], _], State0[_[_]]](base: PersistentOnDemandPropagationModule.Aux[Var0, Val0, Lang0, State0])
extends PropagationListModule[Var0, Val0, Lang0, State0](base) with StashOnDemandPropagationModule {
  override implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): OnDemandPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] = base.freePropagation
}