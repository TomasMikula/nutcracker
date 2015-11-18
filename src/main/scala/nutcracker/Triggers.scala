package nutcracker

import PartialSolution._

case class Triggers(
    domainTriggers: Map[PureDomRef[_, _], List[ProblemDescription[Unit]]],
    domainResolutionTriggers: Map[PureDomRef[_, _], List[_ => ProblemDescription[Unit]]],
    promiseTriggers: Map[Long, List[_ => ProblemDescription[Unit]]]) {

  def addDomainTrigger[A, D](ref: PureDomRef[A, D], pd: ProblemDescription[Unit]): Triggers =
    copy(domainTriggers = domainTriggers + ((ref, pd :: domainTriggers.getOrElse(ref, Nil))))

  def addDomainResolutionTrigger[A, D](ref: PureDomRef[A, D], f: A => ProblemDescription[Unit]): Triggers =
    copy(domainResolutionTriggers = domainResolutionTriggers + ((ref, f :: domainResolutionTriggers.getOrElse(ref, Nil))))

  def addOnComplete[A](pr: PromiseId[A], f: A => ProblemDescription[Unit]): Triggers =
    copy(promiseTriggers = promiseTriggers + ((pr.id, f :: promiseTriggers.getOrElse(pr.id, Nil))))

  def getForDomain[A, D](ref: PureDomRef[A, D]): List[ProblemDescription[Unit]] =
    domainTriggers.getOrElse(ref, Nil)

  def domainResolvedTriggers[A, D](ref: PureDomRef[A, D], a: A): (Triggers, List[ProblemDescription[Unit]]) = {
    val conts = domainResolutionTriggers.getOrElse(ref, Nil).asInstanceOf[List[A => ProblemDescription[Unit]]] map { _(a) }
    (copy(domainResolutionTriggers = domainResolutionTriggers - ref), conts)
  }

  def promiseCompleted[A](pid: PromiseId[A], a: A): (Triggers, List[ProblemDescription[Unit]]) = {
    val conts = promiseTriggers.getOrElse(pid.id, Nil).asInstanceOf[List[A => ProblemDescription[Unit]]] map { _(a) }
    (copy(promiseTriggers = promiseTriggers - pid.id), conts)
  }
}

object Triggers {
  def empty = Triggers(
      domainTriggers = Map(),
      domainResolutionTriggers = Map(),
      promiseTriggers = Map()
  )
}