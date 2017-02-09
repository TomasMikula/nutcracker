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
  type Ref[a] = PropagationStore.module.Ref[a]

  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K] = Const[C, K]

  type Vocabulary[K[_], A] = (PropagationLang[Ref, ?[_], ?]  :+: RelLang :++: CostL)#Out[K, A]
  type State[K]            = (PropagationStore[Ref, ?]       :*: RelDB   :**: CostS)#Out[K]

  val interpreter = (PropagationStore.interpreter[Ref] :&: RelDB.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K]: Lens[State[K], PropagationStore[Ref, K]] = implicitly[Lens[State[K], PropagationStore[Ref, K]]]
  def cost[K]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropRelCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    PropagationStore.naiveAssess(propStore[Q[Unit]])(FreeKT.injectionOrder[PropagationLang[Ref, ?[_], ?], Vocabulary, Id])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?)](pa => s => propStore[Q[Unit]].get(s).fetchResult(pa).get)
  private def getCost: State[Q[Unit]] => C = s => cost[Q[Unit]].get(s).getConst
  private def emptyState: State[Q[Unit]] =
    PropagationStore.module.empty[Q[Unit]] :*: RelDB.empty[Q[Unit]] :**: Const[C, Q[Unit]](NonDecreasingMonoid[C].zero)

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
