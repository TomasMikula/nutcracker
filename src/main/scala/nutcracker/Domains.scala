package nutcracker

import scala.language.{existentials, higherKinds}

import nutcracker.util.Index

import shapeless.{HList, Nat, Sized}
import shapeless.PolyDefns.~>

import Domain._
import Domains._

case class Domains[K[_]] private(
    nextId: Long,
    domains: Map[Long, (D, Domain[A, D]) forSome { type A; type D }],
    domainTriggers: Map[CellRef[_], List[_ => Trigger[K]]],
    selTriggers: Map[Sel[_], List[_ => Trigger[K]]],
    cellsToSels: Index[Sel[_ <: HList], CellRef[_]],
    unresolvedVars: Set[DomRef[A, D] forSome { type A; type D }],
    failedVars: Set[Long]) {

  private val cellFetcher: CellRef ~> shapeless.Id = new ~>[CellRef, shapeless.Id] {
    def apply[D](cell: CellRef[D]): D = fetch(cell)
  }

  def addVariable[A, D](d: D, ev: Domain[A, D]): (Domains[K], DomRef[A, D]) = {
    val domains1 = domains + ((nextId, (d, ev)))
    val ref = DomRef[A, D](nextId)
    val (unresolvedVars1, failedVars1) = ev.values(d) match {
      case Empty() => (unresolvedVars, failedVars + nextId)
      case Just(a) => (unresolvedVars, failedVars)
      case Many(_) => (unresolvedVars + ref, failedVars)
    }
    (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: CellRef[D]): D = ref match {
    case dr@DomRef(_) => getDomain(dr)._1
  }

  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def intersect[D](ref: CellRef[D], d: D): Option[Domains[K]] = ref match {
    case dr @ DomRef(_) => intersect(dr, d)
  }

  def intersectVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]): (Domains[K], List[CellRef[D]]) =
    (refs zip values).foldLeft[(Domains[K], List[CellRef[D]])]((this, Nil)) {
      case ((doms, dirtyCells), (ref, d)) => intersect(ref, d) match {
        case Some(doms1) => (doms1, ref :: dirtyCells)
        case None => (doms, dirtyCells)
      }
    }

  private def intersect[A, D](ref: DomRef[A, D], d: D): Option[Domains[K]] = {
    val (d0, dom) = getDomain(ref)
    val d1 = dom.meet(d0, d)
    if(dom.eqv(d0, d1))
      None
    else {
      val (unresolvedVars1, failedVars1) = dom.values(d1) match {
        case Empty() => (unresolvedVars - ref, failedVars + ref.domainId)
        case Just(_) => (unresolvedVars - ref, failedVars)
        case Many(_) => (unresolvedVars, failedVars)
      }
      val domains1 = domains + ((ref.domainId, (d1, dom)))
      Some(copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1))
    }
  }

  def addDomainTrigger[D](ref: CellRef[D], t: D => Trigger[K]): (Domains[K], Option[K[Unit]]) = {
    t(fetch(ref)) match {
      case Discard() => (this, None)
      case Sleep() => (addDomainTrigger0(ref, t), None)
      case Fire(cont) => (this, Some(cont))
      case FireReload(cont) => (addDomainTrigger0(ref, t), Some(cont))
    }
  }

  private def addDomainTrigger0[D](ref: CellRef[D], t: D => Trigger[K]): Domains[K] =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

  def triggersForDomain[D](ref: CellRef[D]): (Domains[K], List[K[Unit]]) = {
    val d = fetch(ref)
    collectTriggers(d, domainTriggers.getOrElse(ref, Nil).asInstanceOf[List[D => Trigger[K]]]) match {
      case (Nil, conts) => (copy(domainTriggers = domainTriggers - ref), conts)
      case (triggers1, conts) => (copy(domainTriggers = domainTriggers + ((ref, triggers1))), conts)
    }
  }

  def addDomainResolutionTrigger[A, D](ref: DomRef[A, D], f: A => K[Unit]): (Domains[K], Option[K[Unit]]) = {
    val domain = getDomain(ref)._2
    addDomainTrigger(ref, (d: D) => domain.values(d) match {
      case Domain.Empty() => Discard()
      case Domain.Just(a) => Fire(f(a))
      case Domain.Many(_) => Sleep()
    })
  }


  def addSelTrigger[L <: HList](sel: Sel[L], t: L => Trigger[K]): Domains[K] =
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )

  def triggersForSel[L <: HList](sel: Sel[L]): (Domains[K], List[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectTriggers(d, selTriggers.getOrElse(sel, Nil).asInstanceOf[List[L => Trigger[K]]]) match {
      case (Nil, conts) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), conts)
      case (triggers1, conts) => (copy(selTriggers = selTriggers + ((sel, triggers1))), conts)
    }
  }

  def getSelsForCell(ref: CellRef[_]): Set[Sel[_ <: HList]] = cellsToSels.get(ref)


  private[nutcracker] def getDomain[A, D](ref: DomRef[A, D]): (D, Domain[A, D]) =
    domains(ref.domainId).asInstanceOf[(D, Domain[A, D])]

  private[nutcracker] def setDomain[A, D](ref: DomRef[A, D], d: D, dom: Domain[A, D]): Domains[K] =
    copy(domains = domains + ((ref.domainId, (d, dom))))
}

object Domains {
  def empty[K[_]] = Domains[K](
      nextId = 0L,
      domains = Map(),
      domainTriggers = Map(),
      selTriggers = Map(),
      cellsToSels = Index.empty(sel => sel.cells),
      unresolvedVars = Set(),
      failedVars = Set()
  )

  private def collectTriggers[K[_], D](d: D, triggers: List[D => Trigger[K]]): (List[D => Trigger[K]], List[K[Unit]]) =
    triggers match {
      case Nil => (Nil, Nil)
      case t :: ts =>
        val (ts1, conts) = collectTriggers(d, ts)
        t(d) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }
}