package nutcracker.toolkit

import nutcracker.util.CoproductK.:++:
import nutcracker.util.APairK._
import nutcracker.{Assessment, BranchingPropagation, Propagation}
import scala.language.existentials
import scalaz.Id.Id
import scalaz.{Monad, ~>}

trait PropBranchToolkit extends PropagationToolkit with BranchingToolkit

object PropBranchToolkit {
  val instance: PropBranchToolkit = PropBranch
}

object PropBranch extends FreePropagationToolkit with FreeBranchingToolkit with PropBranchToolkit {
  val Prop = PersistentPropagationModule.instance.stashable

  type VarK[K[_], A] = Prop.VarK[K, A]
  type ValK[K[_], A] = Prop.ValK[K, A]

  val Branch = PersistentBranchingModule.instance[VarK, ValK].stashable

  type Lang[K[_], A] = (Prop.Lang   :++: Branch.Lang  )#Out[K, A]
  type StateK[K[_]]  = (Prop.StateK :**: Branch.StateK)#Out[K]

  def varOrderK[K[_]] = Prop.varOrderK
  def varShowK[K[_]] = Prop.varShowK
  def valOrderK[K[_]] = Prop.valOrderK
  def valShowK[K[_]] = Prop.valShowK

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = Prop.readOnlyK(ref)
  override def prgMonad: Monad[Prg] = implicitly

  implicit val propagationApi: Propagation[Prg, Var, Val] =
    Prop.freePropagation[Lang]

  implicit val branchingApi: BranchingPropagation[Prg, Var, Val] =
    Branch.freeBranchingPropagation[Lang]

  import Branch.{stashRestore => sr2}
  import Prop.{stashRestore => sr1}
  def stashRestoreK[K[_]]: StashRestore[StateK[K]] = StashRestore.kPairInstance

  val stepInterpreter = Prop.stepInterpreterK[Prg, State] :+: Branch.stepInterpreter[Prg, State]
  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] = Prop.fetchK(ref, s._1)
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A         = Prop.fetchK(ref, s._1)
  def emptyK[K[_]]: StateK[K] = Prop.emptyK[K] :*: Branch.emptyK[K]

  def assess(s: State): Assessment[List[Prg[Unit]]] =
    Branch.assess(s._2)(
      new (Var ~> Id) {
        override def apply[A](ref: Var[A]): A = Prop.fetchK(ref, s._1)
      }
    )
}
