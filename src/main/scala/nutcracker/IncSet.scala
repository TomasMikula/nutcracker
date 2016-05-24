package nutcracker

/** Increasing set.
  * A wrapper for `Set` where a _monotonic_ update is one that adds
  * elements, e.g. by union with another set.
  */
final class IncSet[A] private(private val value: Set[A]) extends AnyVal {
  def size: Int = value.size
  def contains(a: A): Boolean = value.contains(a)
  def head: A = value.head
  def union(that: IncSet[A]): IncSet[A] = new IncSet(this.value union that.value)
  def toList: List[A] = value.toList
}

object IncSet {
  def apply[A](as: A*): IncSet[A] = new IncSet(Set(as: _*))
  def singleton[A](a: A): IncSet[A] = new IncSet(Set(a))
  def wrap[A](as: Set[A]): IncSet[A] = new IncSet(as)

  type IncSetDom[A] = Dom .Aux[IncSet[A], Join[IncSet[A]], Diff[Set[A]]]
  type IncSetRef[A] = DRef.Aux[IncSet[A], Join[IncSet[A]], Diff[Set[A]]]

  implicit def domInstance[A]: IncSetDom[A] = new Dom[IncSet[A]] {
    type Update = Join[IncSet[A]]
    type Delta = Diff[Set[A]]

    override def assess(d: IncSet[A]): Dom.Status[Join[IncSet[A]]] = d.size match {
      case 0 => Dom.Unrefined(() => None)
      case _ => Dom.Refined
    }

    override def update(s: IncSet[A], u: Join[IncSet[A]]): Option[(IncSet[A], Diff[Set[A]])] = {
      val res = s union u.value
      if(res.size > s.size) Some((res, Diff(u.value.value diff s.value)))
      else None
    }

    override def combineDeltas(d1: Diff[Set[A]], d2: Diff[Set[A]]): Diff[Set[A]] =
      Diff(d1.value union d2.value)
  }

}