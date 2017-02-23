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

  type Prg[A] = FreeK[Vocabulary, A]

  val interpreter = (Prop.interpreter :&: RelDB.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  private def naiveAssess: State[Prg] => Assessment[List[Prg[Unit]]] =
    Prop.naiveAssess(propStore[Prg])
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore[Prg].get(s))(pa).get)
  private def getCost: State[Prg] => C = s => cost[Prg].get(s).value
  private def emptyState: State[Prg] =
    Prop.empty[Prg] :*: RelDB.empty[Prg] :*: CostS(NonDecreasingMonoid[C].zero)

  def dfsSolver: DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)
}
