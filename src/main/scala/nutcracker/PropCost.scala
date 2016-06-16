package nutcracker

import scala.language.higherKinds

import monocle.Lens
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeK, FreeKT}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._

import scalaz.Id._
import scalaz.{Const, ~>}

final class PropCost[C: NonDecreasingMonoid] {
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K] = Const[C, K]

  type Vocabulary[K[_], A] = (PropagationLang  :++: CostL)#Out[K, A]
  type State[K]            = (PropagationStore :**: CostS)#Out[K]

  val interpreter = (PropagationStore.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore[Q[Unit]])(FreeKT.injectionOrder[PropagationLang, Vocabulary, Id])
  private def fetch: Promised ~> (State[Q[Unit]] => ?) = new ~>[Promised, State[Q[Unit]] => ?] {
    def apply[A](pa: Promised[A]): (State[Q[Unit]] => A) = s => propStore[Q[Unit]].get(s).fetchResult(pa).get
  }
  private def getCost: State[Q[Unit]] => C = s => cost[Q[Unit]].get(s).getConst
  private def emptyState: State[Q[Unit]] = PropagationStore.empty[Q[Unit]] :**: Const[C, Q[Unit]](NonDecreasingMonoid[C].zero)

  def dfsSolver: DFSSolver[Vocabulary, State, Id, Promised] = new DFSSolver[Vocabulary, State, Id, Promised](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, Promised, C] = new BFSSolver[Vocabulary, State, Id, Promised, C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
