package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.{FreeK, FreeKT}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._

import scalaz.Id._
import scalaz.~>

object PropRel {
  type Ref[a] = PropagationStore.module.Ref[a]

  type Vocabulary[K[_], A] = (PropagationLang[Ref, ?[_], ?] :++: RelLang)#Out[K, A]
  type State[K]            = (PropagationStore[Ref, ?]      :**: RelDB  )#Out[K]

  val interpreter = (PropagationStore.interpreter[Ref] :&&: RelDB.interpreter).freeInstance
  def propStore: Lens[State[Q[Unit]], PropagationStore[Ref, Q[Unit]]] = implicitly[Lens[State[Q[Unit]], PropagationStore[Ref, Q[Unit]]]]
  def emptyState: State[Q[Unit]] = PropagationStore.module.emptyF[Vocabulary] :**: RelDB.empty[Q[Unit]]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)

  private[PropRel] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore)(FreeKT.injectionOrder[PropagationLang[Ref, ?[_], ?], Vocabulary, Id])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?)](pa => s => propStore.get(s).fetchResult(pa).get)
}
