package nutcracker

trait Dom[D, U, Δ] {

  /** Applies the monotonic update `u` to `d`, obtaining `d1 ≥ d`
    * and a description of the diff. If the update doesn't have any
    * effect on `d`, returns `None`. In addition to being monotonic,
    * updates also have to be _commutative_ and _idempotent_ (when
    * the deltas are combined using [[combineDiffs()]]).
    */
  def update(d: D, u: U): Option[(D, Δ)]

  /** Associative, idempotent and monotonic (non-decreasing) operation
    * to combine diffs.
    */
  def combineDiffs(d1: Δ, d2: Δ): Δ

  def assess(d: D): Dom.Status[U]
}

object Dom {
  sealed trait Status[+U]
  case class Unrefined[U](xor: () => Option[List[U]]) extends Status[U]
  case object Refined extends Status[Nothing]
  case object Failed extends Status[Nothing]
}