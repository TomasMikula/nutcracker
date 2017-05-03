package nutcracker

import scala.language.implicitConversions

trait PropagationBundle extends RefBundle with PropagationToolkit

trait PropagationToolkit extends RefToolkit {
  implicit val propagationApi: Propagation[Prg, Var, Val]

  override implicit def readOnly[A](ref: Var[A]): Val[A] = propagationApi.readOnly(ref)
}

trait OnDemandPropagationToolkit extends PropagationToolkit {
  override implicit val propagationApi: OnDemandPropagation[Prg, Var, Val]
}

object OnDemandPropagationToolkit {
  val instance: OnDemandPropagationToolkit = PropagationImpl
}

trait OnDemandPropagationBundle extends PropagationBundle with OnDemandPropagationToolkit