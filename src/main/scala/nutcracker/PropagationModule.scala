package nutcracker

import nutcracker.util.{FreeK, HEqualK, InjectK, ShowK, StateInterpreter}

trait PropagationModule extends Module {
  type Ref[_]

  implicit def refEquality: HEqualK[Ref]
  implicit def refShow: ShowK[Ref]
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref]

  def interpreter: StateInterpreter[Lang, State]
  def isConsistent[K[_]](s: State[K]): Boolean
  def fetch[K[_], A](s: State[K])(ref: Ref[A]): A

  def fetchResult[K[_], D](s: State[K])(ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out] =
    fin.extract(fetch(s)(ref))
}

trait PersistentPropagationModule extends PropagationModule with PersistentStateModule { self =>
  override def stashable: (StashModule with PropagationModule) {
    type Ref[A] = self.Ref[A]
    type Lang[K[_], A] = self.Lang[K, A]
  }
}

object PersistentPropagationModule {
  type Aux[Ref0[_], Lang0[_[_], _], State0[_[_]]] = PersistentPropagationModule {
    type Ref[A] = Ref0[A]
    type Lang[K[_], A] = Lang0[K, A]
    type State[K[_]] = State0[K]
  }
}

class PropagationListModule[Ref0[_], Lang0[_[_], _], State0[_[_]]](base: PersistentPropagationModule.Aux[Ref0, Lang0, State0])
extends ListModule[Lang0, State0](base) with PropagationModule {
  type Ref[A] = Ref0[A]

  implicit def refEquality: HEqualK[Ref] = base.refEquality
  implicit def refShow: ShowK[Ref] = base.refShow
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] = base.freePropagation

  def interpreter: StateInterpreter[Lang, State] = base.interpreter.inHead
  def isConsistent[K[_]](s: State[K]): Boolean = base.isConsistent[K](s.head)
  def fetch[K[_], A](s: State[K])(ref: Ref[A]): A = base.fetch[K, A](s.head)(ref)
}