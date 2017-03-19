package nutcracker

trait SyncDom[D] extends Dom[D] {
  def toPatch(d: D, δ: Delta): Update
}
