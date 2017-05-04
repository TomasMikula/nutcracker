package nutcracker

import nutcracker.util.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{FreeK, InjectK, StateInterpreter}

trait DeferModule[D] extends Module {
  implicit def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], D]

  def interpreter: StateInterpreter[Lang, StateK]
}

object DeferModule {
  def instance[D](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]): PersistentDeferModule[D] =
    new DeferModuleImpl[D]
}

trait PersistentDeferModule[D] extends DeferModule[D] with PersistentStateModule { self =>
  def stashable: StashDeferModule[D] { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentDeferModule {
  type Aux[D, Lang0[_[_], _], State0[_[_]]] = PersistentDeferModule[D] {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
  }
}

trait StashDeferModule[D] extends DeferModule[D] with StashModule

class DeferListModule[D, Lang[_[_], _], State0[_[_]]](base: PersistentDeferModule.Aux[D, Lang, State0]) extends ListModule[Lang, State0](base) with StashDeferModule[D] {
  def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]) = base.freeDeferApi[F]

  def interpreter: StateInterpreter[Lang, StateK] = base.interpreter.inHead
}