package nutcracker

import nutcracker.util.CoproductK.:++:
import nutcracker.util.FreeK
import nutcracker.util.KPair._

import scalaz.Id.Id
import scalaz.{Lens, ~>}

object PropBranch {
  val Prop = Propagation.module
  val Branch = BranchingPropagation.module(Prop)

  type Ref[a] = Prop.Ref[a]

  type Vocabulary[K[_], A] = (Prop.Lang  :++: Branch.Lang )#Out[K, A]
  type State[K[_]]         = (Prop.State :**: Branch.State)#Out[K]

  type Prg[A] = FreeK[Vocabulary, A]

  implicit val branchingPropagation: BranchingPropagation[Prg, Ref] =
    Branch.freeBranchingPropagation[Vocabulary]
  implicit val propagation: Propagation[Prg, Ref] =
    Prop.freePropagation[Vocabulary]

  val interpreter = (Prop.interpreter :&&: Branch.interpreter).freeInstance
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def branchStore[K[_]]: Lens[State[K], Branch.State[K]] = implicitly[Lens[State[K], Branch.State[K]]]
  def fetch[K[_], A](s: State[K])(ref: Ref[A]): A = Prop.fetch(propStore[K].get(s))(ref)
  def emptyState[K[_]]: State[K] = Prop.empty[K] :*: Branch.empty[K]

  def dfsSolver: DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]](interpreter, emptyState[Prg], naiveAssess, fetch)

  private def naiveAssess: State[Prg] => Assessment[List[Prg[Unit]]] = s =>
    Prop.naiveAssess(propStore[Prg]).apply(s) orElse Branch.assess(branchStore[Prg].get(s))(λ[Ref ~> Id](ref => Prop.fetch(propStore[Prg].get(s))(ref)))
  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore[Prg].get(s))(pa).get)
}
