package nutcracker

trait PropagationBundle extends RefBundle {
  implicit def propagationApi: Propagation[Prg, Ref]
}
