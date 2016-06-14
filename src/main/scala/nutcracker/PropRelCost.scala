package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.{CoproductK, FreeK, FreeKT, ProductK}
import nutcracker.util.ProductK._

import scalaz.Id._
import scalaz.{~>}
import shapeless.Const

final class PropRelCost[C: NonDecreasingMonoid] {
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K] = Const[C]#λ[K]

  type Vocabulary0[K[_], A] = CoproductK[RelLang, CostL, K, A]
  type Vocabulary[K[_], A] = CoproductK[PropagationLang, Vocabulary0, K, A]

  type State0[K] = ProductK[RelDB, CostS, K]
  type State[K] = ProductK[PropagationStore, State0, K]

  val interpreter = (PropagationStore.interpreter :*: RelDB.interpreter :*: CostLang.interpreter[C]).freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropRelCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore[Q[Unit]])(FreeKT.injectionOrder[PropagationLang, Vocabulary, Id])
  private def fetch: Promised ~> (State[Q[Unit]] => ?) = new ~>[Promised, State[Q[Unit]] => ?] {
    def apply[A](pa: Promised[A]): (State[Q[Unit]] => A) = s => propStore[Q[Unit]].get(s).fetchResult(pa).get
  }
  private def getCost: State[Q[Unit]] => C = s => cost[Q[Unit]].get(s)
  private def emptyState: State[Q[Unit]] = PropagationStore.empty[Q[Unit]] :*: RelDB.empty[Q[Unit]] :*: (NonDecreasingMonoid[C].zero: CostS[Q[Unit]])

  def dfsSolver: DFSSolver[Vocabulary, State, Id, Promised] = new DFSSolver[Vocabulary, State, Id, Promised](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, Promised, C] = new BFSSolver[Vocabulary, State, Id, Promised, C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
