package nutcracker.toolkit

import nutcracker.CostApi
import nutcracker.util.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeK, Inject, StateInterpreter}
import scalaz.Lens

trait CostModule[C] extends Module {
  implicit def freeCost[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]): CostApi.Aux[FreeK[F, *], C]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S]

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

class CostListModule[C, Lang0[_[_], _], State0[_[_]]](base: PersistentCostModule.Aux[C, Lang0, State0])
extends ListModule[Lang0, State0](base) with CostModule[C] {
  override type Lang[K[_], A] = Lang0[K, A]

  def freeCost[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]) = base.freeCost[F]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] =
    base.interpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))

  def getCost[K[_]](s: StateK[K]) = base.getCost(s.head)
}