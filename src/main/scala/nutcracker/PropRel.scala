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
  val Prop = PropagationStore.module
  import Prop._

  type Ref[a] = Prop.Ref[a]

  type Vocabulary[K[_], A] = (Prop.Lang  :++: RelLang)#Out[K, A]
  type State[K]            = (Prop.State :**: RelDB  )#Out[K]

  val interpreter = (Prop.interpreter :&&: RelDB.interpreter).freeInstance
  def propStore: Lens[State[Q[Unit]], Prop.State[Q[Unit]]] = implicitly[Lens[State[Q[Unit]], Prop.State[Q[Unit]]]]
  def emptyState: State[Q[Unit]] = Prop.emptyF[Vocabulary] :**: RelDB.empty[Q[Unit]]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)

  private[PropRel] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    Prop.naiveAssess(propStore)(FreeKT.injectionOrder[Prop.Lang, Vocabulary, Id])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?)](pa => s => Prop.fetchResult(propStore.get(s))(pa).get)
}
