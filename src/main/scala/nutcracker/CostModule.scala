package nutcracker

import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeK, InjectK, Step}
import scalaz.NonEmptyList

trait CostModule[C] extends Module {
  implicit def freeCost[F[_[_], _]](implicit i: InjectK[Lang, F]): CostApi.Aux[FreeK[F, ?], C]

  def interpreter: Step[Lang, State]
  def getCost[K[_]](s: State[K]): C
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
    type State[K[_]] = State0[K]
  }
}

class CostListModule[C, Lang[_[_], _], State0[_[_]]](base: PersistentCostModule.Aux[C, Lang, State0])
extends ListModule[Lang, State0](base) with CostModule[C] {
  def freeCost[F[_[_], _]](implicit i: InjectK[Lang, F]) = base.freeCost[F]
  def getCost[K[_]](s: NonEmptyList[State0[K]]) = base.getCost(s.head)
  def interpreter: Step[Lang, State] = base.interpreter.inHead
}