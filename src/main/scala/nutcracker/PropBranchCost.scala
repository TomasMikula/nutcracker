package nutcracker

import scala.language.higherKinds

import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.FreeK
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._

import scalaz.Lens
import scalaz.Id._
import scalaz.~>

final case class PropBranchCost[C: NonDecreasingMonoid]() {
  val Prop = Propagation.module
  val Branch = BranchingPropagation.module(Prop)

  type Ref[a] = Prop.Ref[a]

  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = CostLang.CostS[C, K]

  def CostS[K[_]](c: C): CostS[K] = CostLang.CostS(c)

  type Vocabulary[K[_], A] = (Prop.Lang  :+: Branch.Lang  :++: CostL)#Out[K, A]
  type State[K[_]]         = (Prop.State :*: Branch.State :**: CostS)#Out[K]

  type Prg[A] = FreeK[Vocabulary, A]

  implicit val branchingPropagation: BranchingPropagation[Prg, Ref] =
    Branch.freeBranchingPropagation[Vocabulary]
  implicit val propagation: Propagation[Prg, Ref] =
    Prop.freePropagation[Vocabulary]

  val interpreter = (Prop.interpreter :&: Branch.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def branchStore[K[_]]: Lens[State[K], Branch.State[K]] = implicitly[Lens[State[K], Branch.State[K]]]
  private def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  def dfsSolver: DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState, naiveAssess, fetch)
  def bfsSolver: BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Vocabulary, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, emptyState, naiveAssess, fetch, getCost)

  private def naiveAssess: State[Prg] => Assessment[List[Prg[Unit]]] = s =>
    Prop.naiveAssess(propStore[Prg]).apply(s) orElse Branch.assess(branchStore[Prg].get(s))(λ[Ref ~> Id](ref => Prop.fetch(propStore[Prg].get(s))(ref)))
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore[Prg].get(s))(pa).get)
  private def getCost: State[Prg] => C = s => cost[Prg].get(s).value
  private def emptyState: State[Prg] = Prop.empty[Prg] :*: Branch.empty[Prg] :*: (CostS(NonDecreasingMonoid[C].zero))
}

