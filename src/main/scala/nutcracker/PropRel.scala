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

  type Vocabulary[K[_], A] = (PropagationLang  :++: RelLang)#Out[K, A]
  type State[K]            = (PropagationStore :**: RelDB  )#Out[K]

  val interpreter = (PropagationStore.interpreter :&&: RelDB.interpreter).freeInstance

  private[PropRel] type Q[A] = FreeK[Vocabulary, A]
  def propStore: Lens[State[Q[Unit]], PropagationStore[Q[Unit]]] = implicitly[Lens[State[Q[Unit]], PropagationStore[Q[Unit]]]]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore)(FreeKT.injectionOrder[PropagationLang, Vocabulary, Id])
  private def fetch: λ[A => DRef[Promise[A]]] ~> (State[Q[Unit]] => ?) =
    λ[λ[A => DRef[Promise[A]]] ~> (State[Q[Unit]] => ?)](pa => s => propStore.get(s).fetchResult(pa).get)
  def emptyState: State[Q[Unit]] = PropagationStore.empty[Q[Unit]] :**: RelDB.empty[Q[Unit]]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => DRef[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => DRef[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
}
