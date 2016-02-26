package nutcracker

import monocle.Lens
import nutcracker.Assessment._
import nutcracker.rel.{RelDB, RelLang}

import scala.language.higherKinds

import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scalaz.{Functor, Monoid}

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
  private type Ass[X] = Assessment[List[(X, Q[Unit])]]
  private implicit def assessmentFunctor: Functor[Ass] = new Functor[Ass] {
    def map[A, B](fa: Ass[A])(f: A => B): Ass[B] = fa match {
      case Done => Done
      case Failed => Failed
      case Stuck => Stuck
      case Incomplete(branches) => Incomplete(branches map { case (a, q) => (f(a), q) })
    }
  }
  def naiveAssess: State[Q] => Assessment[List[(State[Q], Q[Unit])]] = propStore[Q].modifyF[Ass](PropagationStore.naiveAssess)
}
