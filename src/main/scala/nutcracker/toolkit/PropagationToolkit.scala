package nutcracker.toolkit

import nutcracker.{OnDemandPropagation, Propagation}

trait PropagationToolkit extends RefToolkit {
  implicit val propagationApi: Propagation.Aux[Prg, Var, Val, Out]

  def run[A](prg: Prg[Out[A]]): A = {
    val (s, out) = interpret0(prg)
    readOut(out, s)
  }
}

object PropagationToolkit {
  val instance: PropagationToolkit = PropagationImpl

  def run[A](
    f: [F[_]] => (propagation: Propagation[F]) => F[propagation.Out[A]]
  ): A =
    instance.run(f(instance.propagationApi))
}

trait FreePropagationToolkit extends FreeRefToolkit with PropagationToolkit

object FreePropagationToolkit {
  val instance: FreePropagationToolkit = PropagationImpl
}

trait OnDemandPropagationToolkit extends PropagationToolkit {
  override implicit val propagationApi: OnDemandPropagation.Aux[Prg, Var, Val, Out]
}

object OnDemandPropagationToolkit {
  val instance: OnDemandPropagationToolkit = PropagationImpl
}

trait FreeOnDemandPropagationToolkit extends FreePropagationToolkit with OnDemandPropagationToolkit

object FreeOnDemandPropagationToolkit {
  val instance: FreeOnDemandPropagationToolkit = PropagationImpl
}