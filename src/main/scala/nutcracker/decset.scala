package nutcracker

import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._

/** Decreasing set.
  * A wrapper for `Set` where a _monotonic_ update is one that removes
  * elements, e.g. by intersection or set difference. This is dual to
  * the usual view of the poset of sets: here, smaller sets (by subset
  * relation) are considered greater (i.e. are higher in the lattice),
  * set intersection is the join and set union is the meet.
  */
final class DecSet[A] private(private val value: Set[A]) extends AnyVal {
  def size: Int = value.size
  def contains(a: A): Boolean = value.contains(a)
  def head: A = value.head
  def tail: DecSet[A] = new DecSet(value.tail)
  def intersect(that: DecSet[A]): DecSet[A] = new DecSet(this.value intersect that.value)
  def diff(that: DecSet[A]): DecSet[A] = new DecSet(this.value diff that.value)
  def toList: List[A] = value.toList
}

object DecSet {
  type Update[A] = Join[DecSet[A]] \/ Diff[DecSet[A]]
  type Delta[A] = Diff[Set[A]]

  def apply[A](as: A*): DecSet[A] = new DecSet(Set(as: _*))
  def singleton[A](a: A): DecSet[A] = new DecSet(Set(a))
  def wrap[A](as: Set[A]): DecSet[A] = new DecSet(as)

  type Dom[A] = JoinDom.Aux[DecSet[A], Update[A], Delta[A]]

  implicit def domInstance[A]: DecSet.Dom[A] = new nutcracker.JoinDom[DecSet[A]] {
    type Update = DecSet.Update[A]
    type Delta = DecSet.Delta[A]

    override def assess(d: DecSet[A]): Dom.Status[Update] = d.size match {
      case 0 => Dom.Failed
      case 1 => Dom.Refined
      case _ => Dom.Unrefined(() => Some(d.toList map (x => Join(singleton(x)).left))) // split into singleton sets
    }

    override def update[S <: DecSet[A]](s: S, u: Update): UpdateResult[DecSet[A], IDelta, S] = u match {
      case -\/(m) => intersect(s, m.value);
      case \/-(d) => diff(s, d.value);
    }

    def ljoin[D <: DecSet[A]](d1: D, d2: DecSet[A]): UpdateResult[DecSet[A], IDelta, D] =
      update(d1, -\/(Join(d2)))

    override def appendDeltas(d1: Delta, d2: Delta): Delta =
      Diff(d1.value union d2.value)

    @inline
    private def intersect[D <: DecSet[A]](a: DecSet[A], b: DecSet[A]): UpdateResult[DecSet[A], IDelta, D] = {
      val res = a intersect b
      if(res.size < a.size) UpdateResult(res, Diff(a.value diff b.value))
      else UpdateResult()
    }

    @inline
    private def diff[D <: DecSet[A]](a: DecSet[A], b: DecSet[A]): UpdateResult[DecSet[A], IDelta, D] = {
      val res = a diff b
      if(res.size < a.size) UpdateResult(res, Diff(a.value intersect b.value))
      else UpdateResult()
    }
  }

  implicit def finalInstance[A]: Final.Aux[DecSet[A], A] = new Final[DecSet[A]] {
    type Out = A

    def extract(d: DecSet[A]): Option[A] = if(d.size == 1) Some(d.head) else None

    def embed(a: A): DecSet[A] = singleton(a)
  }

}