package nutcracker

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

  type DecSetDom[A] = Dom [DecSet[A], Meet[DecSet[A]] \/ Diff[DecSet[A]], Diff[DecSet[A]]]
  type DecSetRef[A] = DRef[DecSet[A], Meet[DecSet[A]] \/ Diff[DecSet[A]], Diff[DecSet[A]]]

  implicit def domInstance[A]: DecSetDom[A] = new DecSetDom[A] {

    override def assess(d: DecSet[A]): Dom.Status[Meet[DecSet[A]] \/ Diff[DecSet[A]]] = d.size match {
      case 0 => Dom.Failed
      case 1 => Dom.Refined
      case _ => Dom.Unrefined(() => Some(d.toList map (x => Meet(singleton(x)).left))) // split into singleton sets
    }

    override def update(s: DecSet[A], u: Meet[DecSet[A]] \/ Diff[DecSet[A]]): Option[(DecSet[A], Diff[DecSet[A]])] = u match {
      case -\/(m) => intersect(s, m.value);
      case \/-(d) => diff(s, d.value);
    }

    override def combineDiffs(d1: Diff[DecSet[A]], d2: Diff[DecSet[A]]): Diff[DecSet[A]] =
      Diff(d1.value union d2.value)

    @inline
    private def intersect(a: DecSet[A], b: DecSet[A]): Option[(DecSet[A], Diff[DecSet[A]])] = {
      val res = a intersect b
      if(res.size < a.size) Some((res, Diff(a diff b)))
      else None
    }

    @inline
    private def diff(a: DecSet[A], b: DecSet[A]): Option[(DecSet[A], Diff[DecSet[A]])] = {
      val res = a diff b
      if(res.size < a.size) Some((res, Diff(a intersect b)))
      else None
    }
  }

  implicit def embedInstance[A]: Embed[A, DecSet[A]] = new Embed[A, DecSet[A]] {
    def embed(a: A): DecSet[A] = singleton(a)
  }

  implicit def extractInstance[A]: Extract[DecSet[A], A] = new Extract[DecSet[A], A] {
    def extract(d: DecSet[A]): Option[A] = if(d.size == 1) Some(d.head) else None
  }

}