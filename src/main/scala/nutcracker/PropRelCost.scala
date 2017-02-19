package nutcracker

import scala.language.higherKinds
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.rel.{RelDB, RelLang}
import nutcracker.util.FreeK
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._

import scalaz.Lens
import scalaz.Id._
import scalaz.~>

final class PropRelCost[C: NonDecreasingMonoid] {
  val Prop = Propagation.module
  import Prop._

  type Ref[a] = Prop.Ref[a]

  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = CostLang.CostS[C, K]

  def CostS[K[_]](c: C): CostS[K] = CostLang.CostS(c)

  type Vocabulary[K[_], A] = (Prop.Lang  :+: RelLang :++: CostL)#Out[K, A]
  type State[K[_]]         = (Prop.State :*: RelDB   :**: CostS)#Out[K]

  val interpreter = (Prop.interpreter :&: RelDB.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private[PropRelCost] type Q[A] = FreeK[Vocabulary, A]
  private def naiveAssess: State[Q] => Assessment[List[Q[Unit]]] =
    Prop.naiveAssess(propStore[Q])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Q] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Q] => ?)](pa => s => Prop.fetchResult(propStore[Q].get(s))(pa).get)
  private def getCost: State[Q] => C = s => cost[Q].get(s).value
  private def emptyState: State[Q] =
    Prop.empty[Q] :*: RelDB.empty[Q] :**: CostS(NonDecreasingMonoid[C].zero)

  def dfsSolver: DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
