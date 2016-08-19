package nutcracker

/** A domain whose deltas can be turned back into updates. */
trait RDom[D] extends Dom[D] {
  def recur(δ: Delta): Update
}

object RDom {
  type Aux[D, U, Δ] = RDom[D] { type Update = U; type Delta = Δ }
}