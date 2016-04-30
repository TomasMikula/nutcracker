package nutcracker

import scalaz.\/

trait Dom[D, U, Δ] {

  /** Applies the monotonic update `u` to `d`, obtaining `d1 ≥ d`
    * and a description of the diff. If the update doesn't have any
    * effect on `d`, returns `None`.
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

  final case class Res[+D](value: D) extends AnyVal
  final case class Meet[+D](value: D) extends AnyVal
  final case class Diff[+D](value: D) extends AnyVal

  type MDom[D] = Dom[D, Meet[D], Unit]
  type CMDom[D] = Dom[D, Meet[D] \/ Diff[D], Res[D] \/ Diff[D]]
  type CMUDom[D] = Dom[D, Meet[D] \/ Diff[D], Unit]
}