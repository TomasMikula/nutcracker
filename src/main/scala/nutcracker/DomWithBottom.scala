package nutcracker

trait DomWithBottom[D] extends Dom[D] {
  def bottom: D
}

object DomWithBottom {
  type Aux[D, U, Δ] = DomWithBottom[D] { type Update = U; type Delta = Δ }
}