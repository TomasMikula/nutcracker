package nutcracker

import scalaz.NonEmptyList

trait Module {
  type Lang[K[_], A]
  type State[K[_]]

  def empty[K[_]]: State[K]
}

trait StashModule extends Module {
  implicit def stashRestore[K[_]]: StashRestore[State[K]]
}

trait PersistentStateModule extends Module { self =>
  def stashable: StashModule { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentStateModule {
  type Aux[Lang0[_[_], _], State0[_[_]]] = PersistentStateModule {
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

class ListModule[Lang0[_[_], _], State0[_[_]]](base: PersistentStateModule.Aux[Lang0, State0]) extends StashModule {
  type Lang[K[_], A] = Lang0[K, A]
  type State[K[_]] = NonEmptyList[State0[K]]

  override def empty[K[_]] =
    NonEmptyList(base.empty[K])

  override implicit def stashRestore[K[_]]: StashRestore[State[K]] =
    StashRestore.nelInstance[State0[K]]
}