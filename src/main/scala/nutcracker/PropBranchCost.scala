package nutcracker

import scala.language.existentials
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.CoproductK._
import nutcracker.util.InjectK
import nutcracker.util.KPair._
import scalaz.Id._
import scalaz.~>

final case class PropBranchCost[C: NonDecreasingMonoid]() extends PropagationBundle with BranchingBundle {
  val Prop = Propagation.module.stashable
  val Branch = BranchingPropagation.module[Prop.Ref].stashable
  val Cost: CostModule[C] with StashModule = CostModule.instance[C].stashable

  type Ref[a] = Prop.Ref[a]

  type Lang[K[_], A] = (Prop.Lang  :+: Branch.Lang  :++: Cost.Lang )#Out[K, A]
  type State[K[_]]   = (Prop.State :*: Branch.State :**: Cost.State)#Out[K]

  implicit def refEquality = Prop.refEquality
  implicit def refShow = Prop.refShow

  implicit val propagationApi: Propagation[Prg, Ref] =
    Prop.freePropagation[Lang]

  implicit val branchingApi: BranchingPropagation[Prg, Ref] =
    Branch.freeBranchingPropagation[Lang]

  implicit val costApi: CostApi.Aux[Prg, C] = {
    // Not sure why scalac is not able to find this itself.
    // Try removing after https://issues.scala-lang.org/browse/SI-10213 is resolved
    implicit val injC: InjectK[Cost.Lang, Lang] =
      InjectK.injectRight[Cost.Lang, (Branch.Lang  :++: Cost.Lang )#Out, Prop.Lang](InjectK.injectRight[Cost.Lang, Cost.Lang, Branch.Lang])

    Cost.freeCost[Lang]
  }

  import Prop.{stashRestore => sr1}
  import Branch.{stashRestore => sr2}
  import Cost.{stashRestore => sr3}
  def stashRestore[K[_]]: StashRestore[State[K]] = StashRestore.kPairInstance

  def empty[K[_]]: State[K] =
    Prop.empty[K] :*: Branch.empty[K] :*: Cost.empty[K]

  def fetch[K[_], A](ref: Ref[A], s: State[K]): A =
    Prop.fetch(ref, s._1)

  val interpreter = (Prop.interpreter :&: Branch.interpreter :&&: Cost.interpreter).freeInstance
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) = interpreter(p).run(s)

  def bfsSolver: BFSSolver[Lang, State, Id, λ[A => Ref[Promise[A]]], C] =
    new BFSSolver[Lang, State, Id, λ[A => Ref[Promise[A]]], C](interpreter, empty[Prg], assess, fetch, getCost)

  def assess(s: State[Prg]): Assessment[List[Prg[Unit]]] =
    if(Prop.isConsistent(s._1))
      Branch.assess(s._2._1)(λ[Ref ~> Id](ref => Prop.fetch(ref, s._1)))
    else
      Assessment.Failed

  private def fetch: λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (State[Prg] => ?)](pa => s => Prop.fetchResult(s._1)(pa).get)
  private def getCost: State[Prg] => C = s => Cost.getCost(s._2._2)
}

