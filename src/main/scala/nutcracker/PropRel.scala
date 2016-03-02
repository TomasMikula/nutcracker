package nutcracker

import scala.language.higherKinds

import monocle.Lens
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.free._
import nutcracker.util.free.ProductK._

import scalaz.~>

object PropRel {

  type Vocabulary[K[_], A] = CoproductK[PropagationLang, RelLang, K, A]
  type State[K[_]] = ProductK[PropagationStore, RelDB, K]
  type Dirty[K[_]] = ProductK[PropagationStore.DirtyThings, RelDB.Dirty, K]

  val interpreter: Interpreter.Aux[Vocabulary, State, Dirty] = implicitly[Interpreter.Aux[Vocabulary, State, Dirty]]
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]

  private[PropRel] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] = PropagationStore.naiveAssess(propStore[Q])
  private def fetch: Promised ~> (State[Q] => ?) = new ~>[Promised, State[Q] => ?] {
    def apply[A](pa: Promised[A]): (State[Q] => A) = s => propStore[Q].get(s).fetchResult(pa).get
  }
  private def emptyState: State[Q] = PropagationStore.empty[Q] :*: RelDB.empty[Q]

  def dfsSolver: DFSSolver[Vocabulary, State, Promised] = new DFSSolver[Vocabulary, State, Promised](interpreter, emptyState, naiveAssess, fetch)
}
