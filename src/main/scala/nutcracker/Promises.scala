package nutcracker

import PartialSolution.PromiseId

case class Promises(
    nextId: Long,
    promises: Map[Long, Option[_]]) {

  def apply[A](pr: PromiseId[A]): Option[A] = promises(pr.id).asInstanceOf[Option[A]]
  def promise[A]: (Promises, PromiseId[A]) = (Promises(nextId + 1, promises + ((nextId, Option.empty))), PromiseId(nextId))
  def complete[A](pid: PromiseId[A], a: A): Promises = copy(promises = promises + ((pid.id, Some(a))))
}

object Promises {
  def empty = Promises(0L, Map())
}