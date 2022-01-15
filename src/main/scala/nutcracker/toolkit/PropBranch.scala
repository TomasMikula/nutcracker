package nutcracker.toolkit

import nutcracker.util.CoproductK.zero
import nutcracker.util.APairK._
import nutcracker.{Assessment, BranchingPropagation, Propagation}
import scalaz.Id.Id
import scalaz.{Monad, ~>}

trait PropBranchToolkit extends PropagationToolkit with BranchingToolkit

object PropBranchToolkit {
  val instance: PropBranchToolkit = PropBranch
}

object PropBranch extends FreePropagationToolkit with FreeBranchingToolkit with PropBranchToolkit {
  val Prop = PersistentPropagationModule.instance.stashable

  override type VarK[K[_], A] = Prop.VarK[K, A]
  override type ValK[K[_], A] = Prop.ValK[K, A]
  override type OutK[K[_], A] = Prop.OutK[K, A]

  val Branch: BranchingModule.Aux0[VarK, ValK] with StashModule =
    PersistentBranchingModule.instance[VarK, ValK].stashable

  val lang   = zero.or [Prop.Lang]  .or [Branch.Lang]
  val stateK = unit.and[Prop.StateK].and[Branch.StateK]

  override type Lang[K[_], A] = lang.Out[K, A]
  override type StateK[K[_]]  = stateK.Out[K]

  override def varOrderK[K[_]] = Prop.varOrderK
  override def varShowK[K[_]] = Prop.varShowK
  override def valOrderK[K[_]] = Prop.valOrderK
  override def valShowK[K[_]] = Prop.valShowK

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = Prop.readOnlyK(ref)
  override def prgMonad: Monad[Prg] = implicitly

  override implicit val propagationApi: Propagation.Aux[Prg, Var, Val, Out] =
    Prop.freePropagation[Lang]

  override implicit val branchingApi: BranchingPropagation.Aux1[Prg, Var, Val] =
    Branch.freeBranchingPropagation[Lang]

  import Branch.{stashRestore => sr2}
  import Prop.{stashRestore => sr1}
  override def stashRestoreK[K[_]]: StashRestore[StateK[K]] = StashRestore.kPairInstance

  override val stepInterpreter = Prop.stepInterpreterK[Prg, State] :+: Branch.stepInterpreter[Prg, State]
  override def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] = Prop.fetchK(ref, s._1)
  override def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A         = Prop.fetchK(ref, s._1)
  override def readOutK[K[_], A](a: OutK[K, A], s: StateK[K]): A         = Prop.readOutK(a, s._1)
  override def emptyK[K[_]]: StateK[K] = Prop.emptyK[K] :*: Branch.emptyK[K]

  override def assess(s: State): Assessment[List[Prg[Unit]]] =
    Branch.assess(s._2)(
      new (Var ~> Id) {
        override def apply[A](ref: Var[A]): A = Prop.fetchK(ref, s._1)
      }
    )
}
