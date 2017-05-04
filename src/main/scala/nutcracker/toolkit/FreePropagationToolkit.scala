package nutcracker.toolkit

import nutcracker.{OnDemandPropagation, Propagation}
import scala.language.implicitConversions

trait FreePropagationToolkit extends FreeRefToolkit with PropagationToolkit

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

trait FreeOnDemandPropagationToolkit extends FreePropagationToolkit with OnDemandPropagationToolkit