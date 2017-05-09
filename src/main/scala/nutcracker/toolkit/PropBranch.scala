package nutcracker.toolkit

import nutcracker.util.CoproductK.:++:
import nutcracker.util.KPair._
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

  type Lang[K[_], A] = (Prop.Lang   :++: Branch.Lang  )#Out1[K, A]
  type StateK[K[_]]  = (Prop.StateK :**: Branch.StateK)#Out[K]

  def varOrderK[K[_]] = Prop.varOrderK
  def varShowK[K[_]] = Prop.varShowK
  def valOrderK[K[_]] = Prop.valOrderK
  def valShowK[K[_]] = Prop.valShowK

  override def prgMonad: Monad[Prg] = implicitly

  implicit val propagationApi: Propagation[Prg, Var, Val] =
    Prop.freePropagation[Lang]

  implicit val branchingApi: BranchingPropagation[Prg, Var, Val] =
    Branch.freeBranchingPropagation[Lang]

  import Branch.{stashRestore => sr2}
  import Prop.{stashRestore => sr1}
  def stashRestoreK[K[_]]: StashRestore[StateK[K]] = StashRestore.kPairInstance

  val interpreter = (Prop.interpreter[Prg, State] :+: Branch.interpreter[Prg, State]).freeInstance(_.unwrap)
  def interpret[A](p: Prg[A], s: State): (State, A) = interpreter(p.unwrap).run(s)
  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] = Prop.fetchK(ref, s._1)
  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A         = Prop.fetchK(ref, s._1)
  def emptyK[K[_]]: StateK[K] = Prop.emptyK[K] :*: Branch.emptyK[K]

  def assess(s: State): Assessment[List[Prg[Unit]]] =
    if (Prop.isConsistent(s._1))
      Branch.assess(s._2)(λ[Var ~> Id](ref => Prop.fetchK(ref, s._1)))
    else
      Assessment.Failed
}
