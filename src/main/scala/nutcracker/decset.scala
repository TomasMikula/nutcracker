package nutcracker

import nutcracker.Dom.{CMDom, Diff, Meet, Res}

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

/** Decreasing set.
  * A wrapper for `Set` where a _monotonic_ update is one that removes
  * elements, e.g. by intersection or set difference.
  */
final class DecSet[A] private(private val value: Set[A]) extends AnyVal {
  def size: Int = value.size
  def contains(a: A): Boolean = value.contains(a)
  def head: A = value.head
  def tail: DecSet[A] = new DecSet(value.tail)
  def intersect(that: DecSet[A]): DecSet[A] = new DecSet(this.value intersect that.value)
  def union(that: DecSet[A]): DecSet[A] = new DecSet(this.value union that.value)
  def diff(that: DecSet[A]): DecSet[A] = new DecSet(this.value diff that.value)
  def toList: List[A] = value.toList
}

object DecSet {
  def apply[A](as: A*): DecSet[A] = new DecSet(Set(as: _*))
  def singleton[A](a: A): DecSet[A] = new DecSet(Set(a))
  def wrap[A](as: Set[A]): DecSet[A] = new DecSet(as)

  implicit def domInstance[A]: CMDom[DecSet[A]] = new CMDom[DecSet[A]] {

    override def assess(d: DecSet[A]): Dom.Status[Meet[DecSet[A]] \/ Diff[DecSet[A]]] = d.size match {
      case 0 => Dom.Failed
      case 1 => Dom.Refined
      case _ => Dom.Unrefined(() => Some(d.toList map (x => Meet(singleton(x)).left))) // split into singleton sets
    }

    override def update(s: DecSet[A], u: Meet[DecSet[A]] \/ Diff[DecSet[A]]): Option[(DecSet[A], Res[DecSet[A]] \/ Diff[DecSet[A]])] = u match {
      case -\/(m) => intersect(s, m.value);
      case \/-(d) => diff(s, d.value);
    }

    override def combineDiffs(
      d1: Res[DecSet[A]] \/ Diff[DecSet[A]],
      d2: Res[DecSet[A]] \/ Diff[DecSet[A]]
    ): Res[DecSet[A]] \/ Diff[DecSet[A]] =
      d2 match {
        case -\/(res2) => d2
        case \/-(diff2) => d1 match {
          case \/-(diff1) => Diff(diff1.value union diff2.value).right
          case -\/(res1) => Res(res1.value diff diff2.value).left
        }
      }

    @inline
    private def intersect(a: DecSet[A], b: DecSet[A]): Option[(DecSet[A], Res[DecSet[A]] \/ Diff[DecSet[A]])] = {
      val res = a intersect b
      if(res.size < a.size) Some((res, Res(res).left))
      else None
    }

    @inline
    private def diff(a: DecSet[A], b: DecSet[A]): Option[(DecSet[A], Res[DecSet[A]] \/ Diff[DecSet[A]])] = {
      val res = a diff b
      if(res.size < a.size) Some((res, Diff(a intersect b).right))
      else None
    }
  }

  implicit def embedExtractInstance[A]: EmbedExtract[A, DecSet[A]] = new EmbedExtract[A, DecSet[A]] {
    def embed(a: A): DecSet[A] = singleton(a)
    def extract(d: DecSet[A]): Option[A] = if(d.size == 1) Some(d.head) else None
  }

}