package nutcracker

import nutcracker.PartialSolution._

import scalaz.Id._
import scalaz.StreamT

object DFSSolver {
  def solutions[A](pd: ProblemDescription[A]): StreamT[Id, A] = {
    val (ps, pr) = PartialSolution.init(pd)
    solutions(ps, pr)
  }
  private def solutions[A](ps: PartialSolution, pr: PromiseId[A]): StreamT[Id, A] = ps.status match {
    case Failed => StreamT.empty
    case Done => ps.getPromised(pr) :: StreamT.empty[Id, A]
    case Incomplete(b::branchings, unresolvedDomains) =>
      ps.branchBy(b) flatMap { solutions(_, pr) }
    case Incomplete(Nil, dRef::dRefs) =>
      ps.splitDomain(dRef) flatMap { solutions(_, pr) }
    case Incomplete(Nil, Nil) => sys.error("Incomplete solution must have at least one unevaluated branching or unresolved variable")
  }
}
