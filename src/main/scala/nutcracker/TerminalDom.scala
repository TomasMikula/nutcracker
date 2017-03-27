package nutcracker

/** Domain whose values can be artificially caused to fail. */
trait TerminalDom[D] extends Dom[D] {

  /** An update that causes any value to fail, i.e.
    *
    *   ∀d. isFailed(update_(d, fail))
    */
  def terminate: Update

}
