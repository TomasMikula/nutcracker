package nutcracker

case class Promises(
    nextId: Long,
    promises: Map[Long, Option[_]],
    promiseTriggers: Map[Long, List[_ => ProblemDescription[Unit]]]) {

  def apply[A](pr: PromiseId[A]): Option[A] = promises(pr.id).asInstanceOf[Option[A]]
  def promise[A]: (Promises, PromiseId[A]) = (copy(nextId = nextId + 1, promises = promises + ((nextId, Option.empty))), PromiseId(nextId))
  def complete[A](pid: PromiseId[A], a: A): (Promises, List[ProblemDescription[Unit]]) = {
    val conts = promiseTriggers.getOrElse(pid.id, Nil).asInstanceOf[List[A => ProblemDescription[Unit]]] map { _(a) }
    (copy(promises = promises + ((pid.id, Some(a))), promiseTriggers = promiseTriggers - pid.id), conts)
  }

  def addTrigger[A](pr: PromiseId[A], f: A => ProblemDescription[Unit]): Promises =
    copy(promiseTriggers = promiseTriggers + ((pr.id, f :: promiseTriggers.getOrElse(pr.id, Nil))))
}

object Promises {
  def empty = Promises(0L, Map(), Map())
}