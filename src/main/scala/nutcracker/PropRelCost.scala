package nutcracker

import monocle.Lens
import nutcracker.rel.{RelDB, RelLang}

import scala.language.higherKinds

import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scalaz.{~>, Applicative, Monoid}

final class PropRelCost[C: Monoid] extends Language {
  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = ConstK[C, K]

  type Vocabulary0[K[_], A] = CoproductK[RelLang, CostL, K, A]
  type Vocabulary[K[_], A] = CoproductK[PropagationLang, Vocabulary0, K, A]

  type State0[K[_]] = ProductK[RelDB, CostS, K]
  type State[K[_]] = ProductK[PropagationStore, State0, K]

  type Dirty0[K[_]] = ProductK[AlwaysClean, AlwaysClean, K]
  type Dirty[K[_]] = ProductK[PropagationStore.DirtyThings, Dirty0, K]

  val interpreter: Interpreter.Aux[Vocabulary, State, Dirty] = implicitly[Interpreter.Aux[Vocabulary, State, Dirty]]
  def propStore[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private type Q[A] = FreeK[Vocabulary, A]
  private type P[A] = FreeK[PropagationLang, A]
  private implicit val pq = implicitly[P ~> Q]
  private implicit val aq = Applicative[Q]
  def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] = s => (PropagationStore.naiveAssess[Q](aq, pq))(propStore[Q].get(s))
}
