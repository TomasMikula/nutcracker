package nutcracker

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

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

  implicit def finiteSetDomain[A]: CMDom[Set[A]] = new CMDom[Set[A]] {

    override def assess(d: Set[A]): Dom.Status[Meet[Set[A]] \/ Diff[Set[A]]] = d.size match {
      case 0 => Dom.Failed
      case 1 => Dom.Refined
      case _ => Dom.Unrefined(() => Some(d.toList map (x => Meet(Set(x)).left))) // split into singleton sets
    }

    override def update(s: Set[A], u: Meet[Set[A]] \/ Diff[Set[A]]): Option[(Set[A], Res[Set[A]] \/ Diff[Set[A]])] = u match {
      case -\/(m) => intersect(s, m.value);
      case \/-(d) => diff(s, d.value);
    }

    override def combineDiffs(d1: Res[Set[A]] \/ Diff[Set[A]], d2: Res[Set[A]] \/ Diff[Set[A]]): Res[Set[A]] \/ Diff[Set[A]] =
      d2 match {
        case -\/(res2) => d2
        case \/-(diff2) => d1 match {
          case \/-(diff1) => Diff(diff1.value union diff2.value).right
          case -\/(res1) => Res(res1.value diff diff2.value).left
        }
      }

    @inline
    private def intersect(a: Set[A], b: Set[A]): Option[(Set[A], Res[Set[A]] \/ Diff[Set[A]])] = {
      val res = a intersect b
      if(res.size < a.size) Some((res, Res(res).left))
      else None
    }

    @inline
    private def diff(a: Set[A], b: Set[A]): Option[(Set[A], Res[Set[A]] \/ Diff[Set[A]])] = {
      val res = a diff b
      if(res.size < a.size) Some((res, Diff(a intersect b).right))
      else None
    }
  }
}