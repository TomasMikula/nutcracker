package nutcracker.toolkit

import nutcracker.CostApi
import nutcracker.util.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeK, Inject, LensK, Step}

trait CostModule[C] extends Module {
  implicit def freeCost[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): CostApi.Aux[FreeK[F, ?], C]

  def interpreter[S[_[_]]](implicit lens: LensK[S, StateK]): Step[Lang, S]

  def getCost[K[_]](s: StateK[K]): C
}

object CostModule {
  def instance[C: NonDecreasingMonoid]: PersistentCostModule[C] = new CostModuleImpl[C]
}

trait PersistentCostModule[C] extends CostModule[C] with PersistentStateModule { self =>
  override def stashable: CostModule[C] with StashModule { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentCostModule {
  type Aux[C, Lang0[_[_], _], State0[_[_]]] = PersistentCostModule[C] {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
  }
}

class CostListModule[C, Lang[_[_], _], State0[_[_]]](base: PersistentCostModule.Aux[C, Lang, State0])
extends ListModule[Lang, State0](base) with CostModule[C] {
  def freeCost[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]) = base.freeCost[F]

  def interpreter[S[_[_]]](implicit lens: LensK[S, StateK]): Step[Lang, S] =
    base.interpreter[S](LensK.compose[S, StateK, State0](LensK.inHead[State0], lens))

  def getCost[K[_]](s: StateK[K]) = base.getCost(s.head)
}