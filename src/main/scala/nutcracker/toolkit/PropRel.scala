package nutcracker.toolkit

import nutcracker.{OnDemandPropagation, Propagation, Relations}
import nutcracker.util.CoproductK._
import nutcracker.util.APairK._
import scalaz.Monad

trait PropRelToolkit extends OnDemandPropagationToolkit with RelToolkit

object PropRelToolkit {
  val instance: PropRelToolkit = PropRel

  def run[A](f: [M[_]] => (P: Propagation[M], R: Relations[M]) => M[P.Out[A]]): A =
    instance.run(f(instance.propagationApi, instance.relationsApi))
}

object PropRel extends FreePropagationToolkit with PropRelToolkit {
  val Prop = OnDemandPropagationModule.instance
  val RelMod = RelModule.instance

  override type VarK[K[_], A] = Prop.VarK[K, A]
  override type ValK[K[_], A] = Prop.ValK[K, A]
  override type OutK[K[_], A] = Prop.OutK[K, A]

  val lang   = zero.or [Prop.Lang]  .or [RelMod.Lang]
  val stateK = unit.and[Prop.StateK].and[RelMod.StateK]

  override type Lang[K[_], A] = lang.Out[K, A]
  override type StateK[K[_]]  = stateK.Out[K]

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = Prop.readOnlyK(ref)

  override implicit def varOrderK[K[_]] = Prop.varOrderK
  override implicit def varShowK[K[_]] = Prop.varShowK
  override implicit def valOrderK[K[_]] = Prop.valOrderK
  override implicit def valShowK[K[_]] = Prop.valShowK

  override def prgMonad: Monad[Prg] = implicitly

  override implicit val propagationApi: OnDemandPropagation.Aux[Prg, Var, Val, Out] =
    Prop.freePropagation[Lang]

  override implicit val relationsApi: Relations[Prg] =
    RelMod.freeRelations[Lang]

  override def emptyK[K[_]]: StateK[K] =
    Prop.emptyK[K] :*: RelMod.emptyK[K]

  override def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] =
    Prop.fetchK(ref, s._1)

  override def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A =
    Prop.fetchK(ref, s._1)

  override def readOutK[K[_], A](a: OutK[K, A], s: StateK[K]): A =
    Prop.readOutK(a, s._1)

  override val stepInterpreter =
    Prop.stepInterpreterK[Prg, StateK[Prg]] :+: RelMod.interpreter[Prg, StateK[Prg]]
}
