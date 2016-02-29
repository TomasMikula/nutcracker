package nutcracker

import nutcracker.Assessment._
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.free.FreeK

import scala.annotation.tailrec
import scalaz._
import scalaz.Id._
import scalaz.std.list._

class BFSSolver[C: NonDecreasingMonoid] {
  val lang: PropRelCost[C] = new PropRelCost[C]

  type K[A] = FreeK[lang.Vocabulary, A]
  type S = lang.State[K]

  implicit val orderByCost: Order[S] = Order.orderBy(s => lang.cost.get(s))

  def solutions[A](p: K[Promised[A]]): StreamT[Id, (A, C)] = {
    lang.interpreter.runFree(p) match {
      case (s, pr) => solutions(s) map { s => (lang.propStore.get(s).fetchResult(pr).get, lang.cost.get(s)) }
    }
  }

  def assess: S => Assessment[List[K[Unit]]] = lang.naiveAssess

  private def solutions(s: S): StreamT[Id, S] = {
    val heap = Heap.singleton[S](s)
    StreamT.unfold(heap)(unfold)
  }

  @tailrec
  private def unfold(heap: Heap[S]): Option[(S, Heap[S])] = heap.uncons match {
    case None => None
    case Some((s, heap1)) => assess(s) match {
      case Failed => unfold(heap1)
      case Stuck => unfold(heap1) // not done, but we don't know how to proceed. TODO: Don't treat as failed
      case Done => Some((s, heap1))
      case Incomplete(sks) =>
        val newStates = sks map { k => lang.interpreter.runFreeUnit(s, k) }
        val heap2 = heap1.insertAllF[List](newStates)
        unfold(heap2)
    }
  }

}
