package nutcracker

import scala.language.existentials

import algebra.lattice.MeetSemilattice
import shapeless.Nat
import shapeless.Sized

import PartialSolution._
import Domain._

case class Domains private(
    nextId: Long,
    domains: Map[Long, (D, Domain[A, D]) forSome { type A; type D }],
    unresolvedVars: Set[PureDomRef[A, D] forSome { type A; type D }],
    failedVars: Set[Long]) {

  def addVariable[A, D](d: D, ev: Domain[A, D]): (Domains, PureDomRef[A, D]) = {
    val domains1 = domains + ((nextId, (d, ev)))
    val ref = PureDomRef[A, D](nextId)
    val (unresolvedVars1, failedVars1) = ev.values(d) match {
      case Empty() => (unresolvedVars, failedVars + nextId)
      case Just(a) => (unresolvedVars, failedVars)
      case Many(_) => (unresolvedVars + ref, failedVars)
    }
    (Domains(nextId + 1, domains1, unresolvedVars1, failedVars1), ref)
  }

  def fetch[A, D](ref: PureDomRef[A, D]): D = getDomain(ref)._1

  def fetchVector[A, D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def intersect[A, D](ref: PureDomRef[A, D], d: D): Option[Domains] = {
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

  private[nutcracker] def getDomain[A, D](ref: PureDomRef[A, D]): (D, Domain[A, D]) =
    domains(ref.domainId).asInstanceOf[(D, Domain[A, D])]

  private[nutcracker] def setDomain[A, D](ref: PureDomRef[A, D], d: D, dom: Domain[A, D]): Domains =
    copy(domains = domains + ((ref.domainId, (d, dom))))
}

object Domains {
  def empty = Domains(
      nextId = 0L,
      domains = Map(),
      unresolvedVars = Set(),
      failedVars = Set()
  )
}