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
  type State[K]            = (PropagationStore[DRef, ?] :**: CostS)#Out[K]

  val interpreter = (PropagationStore.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[DRef, K]] = implicitly[Lens[State[K], PropagationStore[DRef, K]]]
  def cost[K]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore[Q[Unit]])(FreeKT.injectionOrder[PropagationLang, Vocabulary, Id])
  private def fetch: λ[A => DRef[Promise[A]]] ~> (State[Q[Unit]] => ?) =
    λ[λ[A => DRef[Promise[A]]] ~> (State[Q[Unit]] => ?)](pa => s => propStore[Q[Unit]].get(s).fetchResult(pa).get)
  private def getCost: State[Q[Unit]] => C = s => cost[Q[Unit]].get(s).getConst
  private def emptyState: State[Q[Unit]] = PropagationStore.empty[Q[Unit]] :**: Const[C, Q[Unit]](NonDecreasingMonoid[C].zero)

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => DRef[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => DRef[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => DRef[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => DRef[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
