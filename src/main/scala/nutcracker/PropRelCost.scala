package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.{FreeK, FreeKT}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._

import scalaz.Id._
import scalaz.{Const, ~>}

final class PropRelCost[C: NonDecreasingMonoid] {
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K] = Const[C, K]

  type Vocabulary[K[_], A] = (PropagationLang  :+: RelLang :++: CostL)#Out[K, A]
  type State[K]            = (PropagationStore :*: RelDB   :**: CostS)#Out[K]

  val interpreter = (PropagationStore.interpreter :&: RelDB.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropRelCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore[Q[Unit]])(FreeKT.injectionOrder[PropagationLang, Vocabulary, Id])
  private def fetch: Promise.Ref ~> (State[Q[Unit]] => ?) = new (Promise.Ref ~> (State[Q[Unit]] => ?)) {
    def apply[A](pa: Promise.Ref[A]): (State[Q[Unit]] => A) = s => propStore[Q[Unit]].get(s).fetchResult(pa).get
  }
  private def getCost: State[Q[Unit]] => C = s => cost[Q[Unit]].get(s).getConst
  private def emptyState: State[Q[Unit]] =
    PropagationStore.empty[Q[Unit]] :*: RelDB.empty[Q[Unit]] :**: Const[C, Q[Unit]](NonDecreasingMonoid[C].zero)

  def dfsSolver: DFSSolver[Vocabulary, State, Id, Promise.Ref] = new DFSSolver[Vocabulary, State, Id, Promise.Ref](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, Promise.Ref, C] = new BFSSolver[Vocabulary, State, Id, Promise.Ref, C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
