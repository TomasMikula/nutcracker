package nutcracker

import nutcracker.rel.{RelModule, RelToolkit, Relations}
import nutcracker.util.FreeKT
import nutcracker.util.CoproductK._
import nutcracker.util.KPair._
import scalaz.Monad
import scalaz.Id.Id

trait PropRelToolkit extends OnDemandPropagationToolkit with RelToolkit

object PropRelToolkit {
  val instance: PropRelToolkit = PropRel
}

object PropRel extends PropagationBundle with PropRelToolkit {
  val Prop = OnDemandPropagation.module
  val RelMod = RelModule.instance

  type VarK[K[_], A] = Prop.VarK[K, A]
  type ValK[K[_], A] = Prop.ValK[K, A]

  type Lang[K[_], A] = (Prop.Lang  :++: RelMod.Lang )#Out[K, A]
  type StateK[K[_]]  = (Prop.StateK :**: RelMod.StateK)#Out[K]

  implicit def varOrderK[K[_]] = Prop.varOrderK
  implicit def varShowK[K[_]] = Prop.varShowK
  implicit def valOrderK[K[_]] = Prop.valOrderK
  implicit def valShowK[K[_]] = Prop.valShowK

  implicit def prgMonad: Monad[Prg] = FreeKT.freeKTMonad[Lang, Id]

  implicit val propagationApi: OnDemandPropagation[Prg, Var, Val] =
    Prop.freePropagation[Lang]

  implicit val relationsApi: Relations[Prg] =
    RelMod.freeRelations[Lang]

  def emptyK[K[_]]: StateK[K] =
    Prop.emptyK[K] :*: RelMod.emptyK[K]

  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): A =
    Prop.fetchK(ref, s._1)

  def interpret[A](p: Prg[A], s: State): (State, A) =
    interpreter(p).run(s)

  val interpreter = (Prop.interpreter :&&: RelMod.interpreter).freeInstance
}
