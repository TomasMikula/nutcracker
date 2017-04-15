package nutcracker

import nutcracker.util.{FreeK, HEqualK, HOrderK, InjectK, ShowK, StateInterpreter}

trait PropagationModule extends Module {
  type Var[_]
  type Val[_]

  implicit def varEquality: HEqualK[Var]
  implicit def varOrder: HOrderK[Var]
  implicit def varShow: ShowK[Var]
  implicit def valEquality: HEqualK[Val]
  implicit def valOrder: HOrderK[Val]
  implicit def valShow: ShowK[Val]
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Var, Val]

  def interpreter: StateInterpreter[Lang, State]
  def isConsistent[K[_]](s: State[K]): Boolean
  def fetch[K[_], A](ref: Val[A], s: State[K]): A
}

trait PersistentPropagationModule extends PropagationModule with PersistentStateModule { self =>
  override def stashable: StashPropagationModule {
    type Var[A] = self.Var[A]
    type Val[A] = self.Val[A]
    type Lang[K[_], A] = self.Lang[K, A]
  }
}

object PersistentPropagationModule {
  type Aux[Var0[_], Val0[_], Lang0[_[_], _], State0[_[_]]] = PersistentPropagationModule {
    type Var[A] = Var0[A]
    type Val[A] = Val0[A]
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

trait StashPropagationModule extends PropagationModule with StashModule

class PropagationListModule[Var0[_], Val0[_], Lang0[_[_], _], State0[_[_]]](base: PersistentPropagationModule.Aux[Var0, Val0, Lang0, State0])
extends ListModule[Lang0, State0](base) with StashPropagationModule {
  type Var[A] = Var0[A]
  type Val[A] = Val0[A]

  implicit def varEquality: HEqualK[Var] = base.varEquality
  implicit def varOrder: HOrderK[Var] = base.varOrder
  implicit def varShow: ShowK[Var] = base.varShow
  implicit def valEquality: HEqualK[Val] = base.valEquality
  implicit def valOrder: HOrderK[Val] = base.valOrder
  implicit def valShow: ShowK[Val] = base.valShow

  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Var, Val] = base.freePropagation

  def interpreter: StateInterpreter[Lang, State] = base.interpreter.inHead
  def isConsistent[K[_]](s: State[K]): Boolean = base.isConsistent[K](s.head)
  def fetch[K[_], A](ref: Val[A], s: State[K]): A = base.fetch[K, A](ref, s.head)
}

trait OnDemandPropagationModule extends PropagationModule {
  implicit override def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): OnDemandPropagation[FreeK[F, ?], Var, Val]
}

trait PersistentOnDemandPropagationModule extends OnDemandPropagationModule with PersistentPropagationModule { self =>
  override def stashable: StashOnDemandPropagationModule {
    type Var[A] = self.Var[A]
    type Val[A] = self.Val[A]
    type Lang[K[_], A] = self.Lang[K, A]
  }
}

object PersistentOnDemandPropagationModule {
  type Aux[Var0[_], Val0[_], Lang0[_[_], _], State0[_[_]]] = PersistentOnDemandPropagationModule with PersistentPropagationModule.Aux[Var0, Val0, Lang0, State0]
}

trait StashOnDemandPropagationModule extends OnDemandPropagationModule with StashPropagationModule

class OnDemandPropagationListModule[Var0[_], Val0[_], Lang0[_[_], _], State0[_[_]]](base: PersistentOnDemandPropagationModule.Aux[Var0, Val0, Lang0, State0])
extends PropagationListModule[Var0, Val0, Lang0, State0](base) with StashOnDemandPropagationModule {
  override implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): OnDemandPropagation[FreeK[F, ?], Var0, Val0] = base.freePropagation
}