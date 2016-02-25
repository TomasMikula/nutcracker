package nutcracker

import nutcracker.Assessment._
import nutcracker.algebraic.NonDecreasingMonoid

import scala.annotation.tailrec
import scalaz._
import scalaz.Id._
import scalaz.std.list._

class BFSSolver[C: NonDecreasingMonoid] extends Solver[PropRelCost[C], List] {
  val lang: PropRelCost[C] = new PropRelCost[C]

  implicit val orderByCost: Order[S] = Order.orderBy(s => lang.cost.get(s))

  def solutions[A](p: K[Promised[A]]): StreamT[Id, (A, C)] = {
    lang.interpreter.runFree(p) match {
      case (s, pr) => solutions(s, pr)
    }
  }

  def assess(state: S): Assessment[List[(S, K[Unit])]] = lang.naiveAssess(state)

  private def solutions[A](s: S, pr: Promised[A]): StreamT[Id, (A, C)] = {
    val heap = Heap.singleton[S](s)
    StreamT.unfold(heap)(h => unfold(h, pr))
  }

  @tailrec
  private def unfold[A](heap: Heap[S], pr: Promised[A]): Option[((A, C), Heap[S])] = heap.uncons match {
    case None => None
    case Some((s, heap1)) => assess(s) match {
      case Failed => unfold(heap1, pr)
      case Stuck => unfold(heap1, pr) // not done, but we don't know how to proceed. TODO: Don't treat as failed
      case Done => Some(((lang.propStore.get(s).fetchResult(pr).get, lang.cost.get(s)), heap1))
      case Incomplete(sks) =>
        val newStates = sks map { case (s1, k) => lang.interpreter.runFreeUnit(s1, k) }
        val heap2 = heap1.insertAllF[List](newStates)
        unfold(heap2, pr)
    }
  }

}
