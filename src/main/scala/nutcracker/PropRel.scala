package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.{CoproductK, FreeK, FreeKT, ProductK}
import nutcracker.util.ProductK._

import scalaz.Id._
import scalaz.~>

object PropRel {

  type Vocabulary[K[_], A] = CoproductK[PropagationLang, RelLang, K, A]
  type State[K[_]] = ProductK[PropagationStore, RelDB, K]

  val interpreter = (PropagationStore.interpreter :*: RelDB.interpreter).freeInstance

  private[PropRel] type Q[A] = FreeK[Vocabulary, A]
  def propStore: Lens[State[Q], PropagationStore[Q]] = implicitly[Lens[State[Q], PropagationStore[Q]]]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore)(FreeKT.injectionOrder[PropagationLang, Vocabulary, Id])
  private def fetch: Promised ~> (State[Q] => ?) = new ~>[Promised, State[Q] => ?] {
    def apply[A](pa: Promised[A]): (State[Q] => A) = s => propStore.get(s).fetchResult(pa).get
  }
  def emptyState: State[Q] = PropagationStore.empty[Q] :*: RelDB.empty[Q]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, Promised] = new DFSSolver[Vocabulary, State, Id, Promised](interpreter, emptyState, naiveAssess, fetch)
}
