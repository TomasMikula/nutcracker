package nutcracker

import scala.language.higherKinds

import monocle.Lens
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeK, FreeKT}
import nutcracker.util.CoproductK._
import nutcracker.util.KList._

import scalaz.Id._
import scalaz.{Const, ~>}

final case class PropCost[C: NonDecreasingMonoid]() {
  val Prop = PropagationStore.module
  import Prop._

  type Ref[a] = Prop.Ref[a]

  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K] = Const[C, K]

  type Vocabulary[K[_], A] = (Prop.Lang  :++: CostL)#Out[K, A]
  type State[K]            = (Prop.State :**: CostS)#Out[K]

  val interpreter = (Prop.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  private def cost[K]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)

  private[PropCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q[Unit]] => Assessment[List[Q[Unit]]] =
    Prop.naiveAssess(propStore[Q[Unit]])(FreeKT.injectionOrder[Prop.Lang, Vocabulary, Id])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q[Unit]] => ?)](pa => s => Prop.fetchResult(propStore[Q[Unit]].get(s))(pa).get)
  private def getCost: State[Q[Unit]] => C = s => cost[Q[Unit]].get(s).getConst
  private def emptyState: State[Q[Unit]] = Prop.emptyF[Vocabulary] :**: Const[C, Q[Unit]](NonDecreasingMonoid[C].zero)
}
