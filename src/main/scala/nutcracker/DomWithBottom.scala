package nutcracker

trait DomWithBottom[D] extends Dom[D] {
  def bottom: D
}

object DomWithBottom {
  type Aux[D, U, Δ] = DomWithBottom[D] { type Update = U; type Delta = Δ }
}

trait SplittableDomWithBottom[D] extends Splittable[D] with DomWithBottom[D]

object SplittableDomWithBottom {
  type Aux[D, U, Δ] = SplittableDomWithBottom[D] { type Update = U; type Delta = Δ }
}