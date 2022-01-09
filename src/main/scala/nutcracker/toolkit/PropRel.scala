package nutcracker.toolkit

import nutcracker.{OnDemandPropagation, Relations}
import nutcracker.util.CoproductK._
import nutcracker.util.APairK._
import scalaz.Monad

trait PropRelToolkit extends OnDemandPropagationToolkit with RelToolkit

object PropRelToolkit {
  val instance: PropRelToolkit = PropRel
}

object PropRel extends FreePropagationToolkit with PropRelToolkit {
  val Prop = OnDemandPropagationModule.instance
  val RelMod = RelModule.instance

  type VarK[K[_], A] = Prop.VarK[K, A]
  type ValK[K[_], A] = Prop.ValK[K, A]

  val lang   = zero.or [Prop.Lang]  .or [RelMod.Lang]
  val stateK = unit.and[Prop.StateK].and[RelMod.StateK]

  type Lang[K[_], A] = lang.Out[K, A]
  type StateK[K[_]]  = stateK.Out[K]

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = Prop.readOnlyK(ref)

  implicit def varOrderK[K[_]] = Prop.varOrderK
  implicit def varShowK[K[_]] = Prop.varShowK
  implicit def valOrderK[K[_]] = Prop.valOrderK
  implicit def valShowK[K[_]] = Prop.valShowK

  override def prgMonad: Monad[Prg] = implicitly

  implicit val propagationApi: OnDemandPropagation.Aux[Prg, Var, Val] =
    Prop.freePropagation[Lang]

  implicit val relationsApi: Relations[Prg] =
    RelMod.freeRelations[Lang]

  def emptyK[K[_]]: StateK[K] =
    Prop.emptyK[K] :*: RelMod.emptyK[K]

  def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] =
    Prop.fetchK(ref, s._1)

  def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A =
    Prop.fetchK(ref, s._1)

  val stepInterpreter = Prop.stepInterpreterK[Prg, StateK[Prg]] :+: RelMod.interpreter[Prg, StateK[Prg]]
}
