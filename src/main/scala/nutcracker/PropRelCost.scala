package nutcracker

import scala.language.higherKinds

import monocle.Lens
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.free.Interpreter._
import nutcracker.util.free._
import nutcracker.util.free.ProductK._

import scalaz.~>

final class PropRelCost[C: NonDecreasingMonoid] {
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = ConstK[C, K]

  type Vocabulary0[K[_], A] = CoproductK[RelLang, CostL, K, A]
  type Vocabulary[K[_], A] = CoproductK[PropagationLang, Vocabulary0, K, A]

  type State0[K[_]] = ProductK[RelDB, CostS, K]
  type State[K[_]] = ProductK[PropagationStore, State0, K]

  val interpreter: Interpreter.Aux[Vocabulary, State] = implicitly[Interpreter.Aux[Vocabulary, State]]
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropRelCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] = PropagationStore.naiveAssess(propStore[Q])
  private def fetch: Promised ~> (State[Q] => ?) = new ~>[Promised, State[Q] => ?] {
    def apply[A](pa: Promised[A]): (State[Q] => A) = s => propStore[Q].get(s).fetchResult(pa).get
  }
  private def getCost: State[Q] => C = s => cost[Q].get(s)
  private def emptyState: State[Q] = PropagationStore.empty[Q] :*: RelDB.empty[Q] :*: (NonDecreasingMonoid[C].zero: CostS[Q])

  def dfsSolver: DFSSolver[Vocabulary, State, Promised] = new DFSSolver[Vocabulary, State, Promised](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Promised, C] = new BFSSolver[Vocabulary, State, Promised, C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
