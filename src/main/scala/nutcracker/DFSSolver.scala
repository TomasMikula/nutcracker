package nutcracker

import nutcracker.PartialSolution._

import scala.annotation.tailrec
import scalaz.Id._
import scalaz.StreamT

object DFSSolver {

  def solutions[A](pd: ProblemDescription[A]): StreamT[Id, A] = {
    val (ps, pr) = PartialSolution.init(pd)
    solutions(ps, pr)
  }

  private def solutions[A](ps: PartialSolution, pr: PromiseId[A]): StreamT[Id, A] =
    solutions[A, A](ps, pr, StreamT.empty, _ :: StreamT.empty[Id, A])

  def solutions1[A](pd: ProblemDescription[A]): StreamT[Id, (Option[A], Int)] = {
    val (ps, pr) = PartialSolution.init(pd)
    solutions1(ps, pr)
  }

  def allSolutions1[A](pd: ProblemDescription[A]): (List[A], Int) = {
    solutions1(pd).foldLeft((List[A](), 0)) {
      case ((sols, n), (Some(sol), i)) => (sol :: sols, n+i)
      case ((sols, n), (None, i)) => (sols, n+i)
    }
  }

  private def solutions1[A](ps: PartialSolution, pr: PromiseId[A]): StreamT[Id, (Option[A], Int)] = {
    val leafs = solutions[A, Option[A]](ps, pr, None :: StreamT.empty[Id, Option[A]], Some(_) :: StreamT.empty[Id, Option[A]])

    def takeOne(s: StreamT[Id, Option[A]]): Option[((Option[A], Int), StreamT[Id, Option[A]])] = takeOne0(s, 0)

    @tailrec def takeOne0(s: StreamT[Id, Option[A]], acc: Int): Option[((Option[A], Int), StreamT[Id, Option[A]])] = s.uncons match {
      case Some((Some(a), tail)) => Some(((Some(a), acc), tail))
      case Some((None, tail)) => takeOne0(tail, acc+1)
      case None =>
        if(acc > 0) Some(((None, acc), StreamT.empty))
        else None
    }

    StreamT.unfold[(Option[A], Int), StreamT[Id, Option[A]]](leafs){ takeOne(_) }
  }

  private def solutions[A, B](
      ps: PartialSolution,
      pr: PromiseId[A],
      failed: StreamT[Id, B],
      done: A => StreamT[Id, B]): StreamT[Id, B] =
    ps.status match {
      case Failed => failed
      case Done => done(ps.getPromised(pr))
      case Incomplete(b::branchings, unresolvedDomains) =>
        ps.branchBy(b) flatMap { solutions(_, pr, failed, done) }
      case Incomplete(Nil, dRef::dRefs) =>
        ps.splitDomain(dRef) flatMap { solutions(_, pr, failed, done) }
      case Incomplete(Nil, Nil) =>
        sys.error("Incomplete solution must have at least one unevaluated branching or unresolved variable")
    }
}
