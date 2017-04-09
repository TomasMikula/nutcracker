package nutcracker

trait PropagationBundle extends RefBundle with PropagationToolkit

trait PropagationToolkit extends RefToolkit {
  implicit val propagationApi: Propagation[Prg, Var, Val]
}