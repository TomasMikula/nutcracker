package nutcracker

import scalaz.NonEmptyList

/** Module bridges three levels of abstraction:
  *  - _API._ Client code is written against an (MTL-style) API.
  *      What API a module provides is module-specific. For example,
  *      [[PropagationModule]] [[PropagationModule#freePropagation provides]]
  *      the [[Propagation]] API.
  *  - _Instruction set._ Low-level instructions which are able to express all
  *      of the API operations. We define _program_ as the free monad over
  *      (a superset of) the instruction set. We consider a variation of free
  *      monads, [[util.FreeK]], where the instructions can talk about
  *      (the type of) programs in which they are embedded.
  *      In the simplest case, there is a one-to-one mapping between the API
  *      operations and instructions. There are cases, however, when an API
  *      operation has to be split into multiple instructions.
  *  - _Interpreter._ Interprets (free) programs in terms of state transitions.
 */
trait Module {
  /** Instructions set.
    *
    * @tparam K allows instructions to refer to the type of program in which
    *           they are embedded. That is, `K` will be instantiated into
    *           `FreeK[F, ?]`, where `F[_[_], _]` is a superset of this
    *           instruction set (i.e. there is an injection from [[Lang]]
    *           to `F`).
    * @tparam A
    */
  type Lang[K[_], A]

  /** State that the interpreter operates on.
    *
    * @tparam K Some states need store programs or program-producing functions.
    *           This type parameter allows the state to talk about the type of
    *           programs.
    *           It will be instantiated into `FreeK[F, ?]`, where `F[_[_], _]`
    *           is a superset of the instruction set [[Lang]] (i.e. there is an
    *           injection from [[Lang]] to `F`).
    */
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