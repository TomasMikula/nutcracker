package nutcracker.toolkit

import nutcracker.{OnDemandPropagation, Propagation}

trait PropagationToolkit extends RefToolkit {
  implicit val propagationApi: Propagation[Prg, Var, Val]
}

object PropagationToolkit {
  val instance: PropagationToolkit = PropagationImpl
}

trait FreePropagationToolkit extends FreeRefToolkit with PropagationToolkit

object FreePropagationToolkit {
  val instance: FreePropagationToolkit = PropagationImpl
}

trait OnDemandPropagationToolkit extends PropagationToolkit {
  override implicit val propagationApi: OnDemandPropagation[Prg, Var, Val]
}

object OnDemandPropagationToolkit {
  val instance: OnDemandPropagationToolkit = PropagationImpl
}

trait FreeOnDemandPropagationToolkit extends FreePropagationToolkit with OnDemandPropagationToolkit

object FreeOnDemandPropagationToolkit {
  val instance: FreeOnDemandPropagationToolkit = PropagationImpl
}