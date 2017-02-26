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