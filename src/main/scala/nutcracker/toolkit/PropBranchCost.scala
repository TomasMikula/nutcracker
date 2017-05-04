package nutcracker.toolkit

import nutcracker.util.CoproductK._
import nutcracker.util.InjectK
import nutcracker.util.KPair._
import nutcracker.util.algebraic.NonDecreasingMonoid
import nutcracker.{Assessment, BranchingPropagation, CostApi, Propagation}
import scala.language.existentials
import scalaz.Id._
import scalaz.{Monad, ~>}

trait PropBranchCostToolkit[C] extends PropBranchToolkit with CostRefToolkit[C]

object PropBranchCostToolkit {
  def instance[C: NonDecreasingMonoid]: PropBranchCostToolkit[C] = new PropBranchCost
}

final class PropBranchCost[C](implicit C: NonDecreasingMonoid[C]) extends PropBranchCostToolkit[C] with FreePropagationToolkit with FreeBranchingToolkit {
  val Prop = PersistentPropagationModule.instance.stashable
  val Branch = PersistentBranchingModule.instance[Prop.VarK, Prop.ValK].stashable
  val Cost: CostModule[C] with StashModule = CostModule.instance[C].stashable

  override def prgMonad: Monad[Prg] = Monad[Prg]
  override def costMonoid: NonDecreasingMonoid[C] = C

  type VarK[K[_], A] = Prop.VarK[K, A]
  type ValK[K[_], A] = Prop.ValK[K, A]

  type Lang[K[_], A] = (Prop.Lang   :+: Branch.Lang   :++: Cost.Lang  )#Out[K, A]
  type StateK[K[_]]  = (Prop.StateK :*: Branch.StateK :**: Cost.StateK)#Out[K]

  implicit def varOrderK[K[_]] = Prop.varOrderK
  implicit def varShowK[K[_]] = Prop.varShowK
  implicit def valOrderK[K[_]] = Prop.valOrderK
  implicit def valShowK[K[_]] = Prop.valShowK

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

  import Branch.{stashRestore => sr2}
  import Cost.{stashRestore => sr3}
  import Prop.{stashRestore => sr1}
  def stashRestoreK[K[_]]: StashRestore[StateK[K]] = StashRestore.kPairInstance

  def emptyK[K[_]]: StateK[K] =
    Prop.emptyK[K] :*: Branch.emptyK[K] :*: Cost.emptyK[K]

  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] =
    Prop.fetchK(ref, s._1)

  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A =
    Prop.fetchK(ref, s._1)

  val interpreter = (Prop.interpreter :&: Branch.interpreter :&&: Cost.interpreter).freeInstance
  def interpret[A](p: Prg[A], s: State): (State, A) = interpreter(p).run(s)

  def assess(s: State): Assessment[List[Prg[Unit]]] =
    if (Prop.isConsistent(s._1))
      Branch.assess(s._2._1)(λ[Var ~> Id](ref => Prop.fetchK(ref, s._1)))
    else
      Assessment.Failed

  def getCost(s: State): C = Cost.getCost(s._2._2)
}

