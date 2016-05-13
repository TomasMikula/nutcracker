package nutcracker

trait Dom[D] {
  type Update
  type Delta

  /** Applies the monotonic update `u` to `d`, obtaining `d1 ≥ d`
    * and a description of the diff. If the update doesn't have any
    * effect on `d`, returns `None`. In addition to being monotonic,
    * updates also have to be _commutative_ and _idempotent_ (when
    * the deltas are combined using [[combineDeltas()]]).
    */
  def update(d: D, u: Update): Option[(D, Delta)]

  /** Associative, idempotent and monotonic (non-decreasing) operation
    * to combine diffs.
    */
  def combineDeltas(d1: Delta, d2: Delta): Delta

  def assess(d: D): Dom.Status[Update]
}

object Dom {
  type Aux[D, U, Δ] = Dom[D] { type Update = U; type Delta = Δ }

  sealed trait Status[+U]
  case class Unrefined[U](xor: () => Option[List[U]]) extends Status[U]
  case object Refined extends Status[Nothing]
  case object Failed extends Status[Nothing]
}