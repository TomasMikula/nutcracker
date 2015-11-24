package nutcracker

import PartialSolution._
import nutcracker.util.Index
import shapeless.HList

import Triggers._

case class Triggers(
    domainTriggers: Map[CellRef[_], List[_ => Trigger]],
    selTriggers: Map[Sel[_], List[_ => Trigger]],
    cellsToSels: Index[Sel[_ <: HList], CellRef[_]],
    domainResolutionTriggers: Map[PureDomRef[_, _], List[_ => ProblemDescription[Unit]]],
    promiseTriggers: Map[Long, List[_ => ProblemDescription[Unit]]]) {

  def addDomainTrigger[D](ref: CellRef[D], t: D => Trigger): Triggers =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

  def getForDomain[D](ref: CellRef[D], d: D): (Triggers, List[ProblemDescription[Unit]]) = {
    collectTriggers(d, domainTriggers.getOrElse(ref, Nil).asInstanceOf[List[D => Trigger]]) match {
      case (Nil, conts) => (copy(domainTriggers = domainTriggers - ref), conts)
      case (triggers1, conts) => (copy(domainTriggers = domainTriggers + ((ref, triggers1))), conts)
    }
  }


  def addSelTrigger[L <: HList](sel: Sel[L], t: L => Trigger): Triggers =
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )

  def getForSel[L <: HList](sel: Sel[L], d: L): (Triggers, List[ProblemDescription[Unit]]) = {
    collectTriggers(d, selTriggers.getOrElse(sel, Nil).asInstanceOf[List[L => Trigger]]) match {
      case (Nil, conts) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), conts)
      case (triggers1, conts) => (copy(selTriggers = selTriggers + ((sel, triggers1))), conts)
    }
  }

  def getSelsForCell(ref: CellRef[_]): Set[Sel[_ <: HList]] = cellsToSels.get(ref)


  def addDomainResolutionTrigger[A, D](ref: PureDomRef[A, D], f: A => ProblemDescription[Unit]): Triggers =
    copy(domainResolutionTriggers = domainResolutionTriggers + ((ref, f :: domainResolutionTriggers.getOrElse(ref, Nil))))

  def domainResolvedTriggers[A, D](ref: PureDomRef[A, D], a: A): (Triggers, List[ProblemDescription[Unit]]) = {
    val conts = domainResolutionTriggers.getOrElse(ref, Nil).asInstanceOf[List[A => ProblemDescription[Unit]]] map { _(a) }
    (copy(domainResolutionTriggers = domainResolutionTriggers - ref), conts)
  }


  def addOnComplete[A](pr: PromiseId[A], f: A => ProblemDescription[Unit]): Triggers =
    copy(promiseTriggers = promiseTriggers + ((pr.id, f :: promiseTriggers.getOrElse(pr.id, Nil))))

  def promiseCompleted[A](pid: PromiseId[A], a: A): (Triggers, List[ProblemDescription[Unit]]) = {
    val conts = promiseTriggers.getOrElse(pid.id, Nil).asInstanceOf[List[A => ProblemDescription[Unit]]] map { _(a) }
    (copy(promiseTriggers = promiseTriggers - pid.id), conts)
  }

  private def collectTriggers[D](d: D, triggers: List[D => Trigger]): (List[D => Trigger], List[ProblemDescription[Unit]]) =
    triggers match {
      case Nil => (Nil, Nil)
      case t :: ts =>
        val (ts1, conts) = collectTriggers(d, ts)
        t(d) match {
          case Discard => (ts1, conts)
          case Sleep => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }
}

object Triggers {

  sealed trait Trigger
  case object Discard extends Trigger
  case object Sleep extends Trigger
  case class Fire(cont: ProblemDescription[Unit]) extends Trigger
  case class FireReload(cont: ProblemDescription[Unit]) extends Trigger

  def empty = Triggers(
      domainTriggers = Map(),
      selTriggers = Map(),
      cellsToSels = Index.empty(sel => sel.cells),
      domainResolutionTriggers = Map(),
      promiseTriggers = Map()
  )
}