package nutcracker

import scala.language.higherKinds

import monocle.Lens
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeK, FreeKT}
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._

import scalaz.Id._
import scalaz.~>

final case class PropCost[C: NonDecreasingMonoid]() {
  val Prop = Propagation.module
  import Prop._

  type Ref[a] = Prop.Ref[a]

  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = CostLang.CostS[C, K]

  def CostS[K[_]](c: C): CostS[K] = CostLang.CostS(c)

  type Vocabulary[K[_], A] = (Prop.Lang  :++: CostL)#Out[K, A]
  type State[K[_]]         = (Prop.State :**: CostS)#Out[K]

  val interpreter = (Prop.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = /*fstLens[Prop.State, CostS, K]*/ implicitly[Lens[State[K], Prop.State[K]]]
  private def cost[K[_]]: Lens[State[K], CostS[K]]   = sndLens[Prop.State, CostS, K] // implicitly[Lens[State[K], CostS[K]]]

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)

  private[PropCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] =
    Prop.naiveAssess(propStore[Q])(FreeKT.injectionOrder[Prop.Lang, Vocabulary, Id])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q] => ?)](pa => s => Prop.fetchResult(propStore[Q].get(s))(pa).get)
  private def getCost: State[Q] => C = s => cost[Q].get(s).value
  private def emptyState: State[Q] = Prop.emptyF[Vocabulary] :**: (CostS(NonDecreasingMonoid[C].zero))
}
