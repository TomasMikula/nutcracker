package nutcracker

import nutcracker.util.CoproductK.:++:
import nutcracker.util.{FreeKT}
import nutcracker.util.KPair._
import scala.language.existentials
import scalaz.Id.Id
import scalaz.{Monad, ~>}

trait PropBranchToolkit extends PropagationToolkit with BranchingToolkit {
  implicit def prgMonad: Monad[Prg]
}

object PropBranchToolkit {
  val instance: PropBranchToolkit = PropBranch
}

object PropBranch extends PropagationBundle with BranchingBundle with PropBranchToolkit {
  val Prop = Propagation.module.stashable
  val Branch = BranchingPropagation.module[Prop.Ref].stashable

  type Ref[a] = Prop.Ref[a]

  type Lang[K[_], A] = (Prop.Lang  :++: Branch.Lang )#Out[K, A]
  type State[K[_]]   = (Prop.State :**: Branch.State)#Out[K]

  implicit def refEquality = Prop.refEquality
  implicit def refShow = Prop.refShow
  implicit def prgMonad = FreeKT.freeKTMonad

  implicit val propagationApi: Propagation[Prg, Ref] =
    Prop.freePropagation[Lang]

  implicit val branchingApi: BranchingPropagation[Prg, Ref] =
    Branch.freeBranchingPropagation[Lang]

  import Prop.{stashRestore => sr1}
  import Branch.{stashRestore => sr2}
  def stashRestore[K[_]]: StashRestore[State[K]] = StashRestore.kPairInstance

  val interpreter = (Prop.interpreter :&&: Branch.interpreter).freeInstance
  def interpret[A](p: Prg[A], s: State[Prg]): (State[Prg], A) = interpreter(p).run(s)
  def fetch[K[_], A](ref: Ref[A], s: State[K]): A = Prop.fetch(ref, s._1)
  def empty[K[_]]: State[K] = Prop.empty[K] :*: Branch.empty[K]

  def assess(s: State[Prg]): Assessment[List[Prg[Unit]]] =
    if(Prop.isConsistent(s._1))
      Branch.assess(s._2)(λ[Ref ~> Id](ref => Prop.fetch(ref, s._1)))
    else
      Assessment.Failed
}
