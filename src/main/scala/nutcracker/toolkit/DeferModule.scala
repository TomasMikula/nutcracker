package nutcracker.toolkit

import nutcracker.Defer
import nutcracker.util.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{FreeK, Inject, StateInterpreter}
import scalaz.Lens

trait DeferModule[D] extends Module {
  implicit def freeDeferApi[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]): Defer[FreeK[F, *], D]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S]
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
  def freeDeferApi[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]) = base.freeDeferApi[F]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] =
    base.interpreter[K, S](Lens.nelHeadLens[State0[K]].compose(lens))
}