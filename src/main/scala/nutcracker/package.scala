package nutcracker {

  /** Used as a monotonic update on domain D:
    * represents the operation of lattice meet with the given value.
    */
  final case class Meet[+D](value: D) extends AnyVal

  /** Used as a monotonic update on domain D:
    * represents the operation of lattice join with the given value.
    */
  final case class Join[D](value: D) extends AnyVal

  /** When used as a monotonic update, represents the operation of relative complement.
    * When used as a delta, represents the part that was removed.
    * An operation `Diff(d)` applied to value `d0` will result in the new value being
    * `d0 \ d` and the published delta being `Diff(d0 ∧ d)`.
    */
  final case class Diff[+D](value: D) extends AnyVal

}