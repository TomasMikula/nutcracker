package nutcracker

import scala.language.existentials
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.CoproductK._
import nutcracker.util.InjectK
import nutcracker.util.KPair._
import scalaz.Id._
import scalaz.{Monad, ~>}

trait PropBranchCostToolkit[C] extends PropBranchToolkit with CostRefToolkit[C]

object PropBranchCostToolkit {
  def instance[C: NonDecreasingMonoid]: PropBranchCostToolkit[C] = new PropBranchCost
}

final class PropBranchCost[C](implicit C: NonDecreasingMonoid[C]) extends PropBranchCostToolkit[C] with PropagationBundle with BranchingBundle {
  val Prop = Propagation.module.stashable
  val Branch = BranchingPropagation.module[Prop.Var, Prop.Val].stashable
  val Cost: CostModule[C] with StashModule = CostModule.instance[C].stashable

  override def prgMonad: Monad[Prg] = Monad[Prg]
  override def costMonoid: NonDecreasingMonoid[C] = C

  type Var[a] = Prop.Var[a]
  type Val[a] = Prop.Val[a]

  type Lang[K[_], A] = (Prop.Lang  :+: Branch.Lang  :++: Cost.Lang )#Out[K, A]
  type State[K[_]]   = (Prop.State :*: Branch.State :**: Cost.State)#Out[K]

  implicit def varEquality = Prop.varEquality
  implicit def varOrder = Prop.varOrder
  implicit def varShow = Prop.varShow
  implicit def valEquality = Prop.valEquality
  implicit def valOrder = Prop.valOrder
  implicit def valShow = Prop.valShow

  implicit val propagationApi: Propagation[Prg, Var, Val] =
    Prop.freePropagation[Lang]

  implicit val branchingApi: BranchingPropagation[Prg, Var, Val] =
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

  def fetch[K[_], A](ref: Val[A], s: State[K]): A =
    Prop.fetch(ref, s._1)

  val interpreter = (Prop.interpreter :&: Branch.interpreter :&&: Cost.interpreter).freeInstance
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) = interpreter(p).run(s)

  def assess(s: State[Prg]): Assessment[List[Prg[Unit]]] =
    if (Prop.isConsistent(s._1))
      Branch.assess(s._2._1)(λ[Var ~> Id](ref => Prop.fetch(ref, s._1)))
    else
      Assessment.Failed

  def getCost(s: State[Prg]): C = Cost.getCost(s._2._2)
}

