package nutcracker

import nutcracker.util.CoproductK.:++:
import nutcracker.util.KPair._

import scalaz.Id.Id
import scalaz.{Lens, ~>}

object PropBranch extends PropagationBundle {
  val Prop = Propagation.module
  val Branch = BranchingPropagation.module[Prop.Ref]

  type Ref[a] = Prop.Ref[a]

  type Lang[K[_], A] = (Prop.Lang  :++: Branch.Lang )#Out[K, A]
  type State[K[_]]   = (Prop.State :**: Branch.State)#Out[K]

  implicit def refEquality = Prop.refEquality
  implicit def refShow = Prop.refShow

  implicit val propagationApi: Propagation[Prg, Ref] =
    Prop.freePropagation[Lang]

  implicit val branchingPropagation: BranchingPropagation[Prg, Ref] =
    Branch.freeBranchingPropagation[Lang]

  val interpreter = (Prop.interpreter :&&: Branch.interpreter).freeInstance
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) = interpreter(p).run(s)
  def propStore[K[_]]: Lens[State[K], Prop.State[K]] = implicitly[Lens[State[K], Prop.State[K]]]
  def branchStore[K[_]]: Lens[State[K], Branch.State[K]] = implicitly[Lens[State[K], Branch.State[K]]]
  def fetch[K[_], A](s: State[K])(ref: Ref[A]): A = Prop.fetch(propStore[K].get(s))(ref)
  def empty[K[_]]: State[K] = Prop.empty[K] :*: Branch.empty[K]

  def dfsSolver: DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]] =
    new DFSSolver[Prg, State, Id, λ[A => Ref[Promise[A]]]](interpreter, empty[Prg], naiveAssess, fetch)

  private def naiveAssess: State[Prg] => Assessment[List[Prg[Unit]]] = s =>
    if(Prop.isConsistent(propStore[Prg].get(s)))
      Branch.assess(branchStore[Prg].get(s))(λ[Ref ~> Id](ref => Prop.fetch(propStore[Prg].get(s))(ref)))
    else
      Assessment.Failed

  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(propStore[Prg].get(s))(pa).get)
}
