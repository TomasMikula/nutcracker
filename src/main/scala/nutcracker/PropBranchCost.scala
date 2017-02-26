package nutcracker

import scala.language.higherKinds
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._

import scalaz.Lens
import scalaz.Id._
import scalaz.~>

final case class PropBranchCost[C: NonDecreasingMonoid]() extends PropagationBundle {
  val Prop = Propagation.module
  val Branch = BranchingPropagation.module[Prop.Ref]

  type Ref[a] = Prop.Ref[a]

  type CostL[K[_], A] = CostLang[C, K, A]
  type CostS[K[_]] = CostLang.CostS[C, K]

  def CostS[K[_]](c: C): CostS[K] = CostLang.CostS(c)

  type Lang[K[_], A] = (Prop.Lang  :+: Branch.Lang  :++: CostL)#Out[K, A]
  type State[K[_]]   = (Prop.State :*: Branch.State :**: CostS)#Out[K]

  implicit def refEquality = Prop.refEquality
  implicit def refShow = Prop.refShow

  implicit val propagationApi: Propagation[Prg, Ref] =
    Prop.freePropagation[Lang]
  implicit val branchingPropagation: BranchingPropagation[Prg, Ref] =
    Branch.freeBranchingPropagation[Lang]

  def empty[K[_]]: State[K] =
    Prop.empty[K] :*: Branch.empty[K] :*: (CostS(NonDecreasingMonoid[C].zero))

  def fetch[K[_], A](s: State[K])(ref: Ref[A]): A =
    Prop.fetch(s._1)(ref)

  val interpreter = (Prop.interpreter :&: Branch.interpreter :&&: CostLang.interpreter[C]).freeInstance
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) = interpreter(p).run(s)
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def branchStore[K[_]]: Lens[State[K], Branch.State[K]] = implicitly[Lens[State[K], Branch.State[K]]]
  private def cost[K[_]]: Lens[State[K], CostS[K]] = implicitly[Lens[State[K], CostS[K]]]

  def dfsSolver: DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]](interpreter, empty[Prg], naiveAssess, fetch)
  def bfsSolver: BFSSolver[Lang, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Lang, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, empty[Prg], naiveAssess, fetch, getCost)

  private def naiveAssess: State[Prg] => Assessment[List[Prg[Unit]]] = s =>
    if(Prop.isConsistent(propStore[Prg].get(s)))
      Branch.assess(branchStore[Prg].get(s))(λ[Ref ~> Id](ref => Prop.fetch(propStore[Prg].get(s))(ref)))
    else
      Assessment.Failed

  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore[Prg].get(s))(pa).get)
  private def getCost: State[Prg] => C = s => cost[Prg].get(s).value
}

