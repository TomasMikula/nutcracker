package nutcracker

/** A domain whose deltas can be turned back into updates. */
trait RDom[D] extends SyncDom[D] {
  def recur(δ: Delta): Update

  override def toPatch(d: D, δ: Delta): Update = recur(δ)
}

object RDom {
  type Aux[D, U, Δ] = RDom[D] { type Update = U; type Delta = Δ }
}