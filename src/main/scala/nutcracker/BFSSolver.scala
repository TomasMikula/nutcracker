package nutcracker

import nutcracker.Assessment._
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.free.{Interpreter, FreeK}

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz._
import scalaz.Id._
import scalaz.std.list._

class BFSSolver[F[_[_], _], St[_[_]], P[_], C: NonDecreasingMonoid](
  interpreter: Interpreter[F] { type State[K[_]] = St[K] },
  initialState: St[FreeK[F, ?]],
  assess: St[FreeK[F, ?]] => Assessment[List[FreeK[F, Unit]]],
  fetch: P ~> (St[FreeK[F, ?]] => ?),
  getCost: St[FreeK[F, ?]] => C
) {
  val lang: PropRelCost[C] = new PropRelCost[C]

  type K[A] = FreeK[F, A]
  type S = St[K]

  implicit val orderByCost: Order[S] = Order.orderBy(getCost)

  def solutions[A](p: K[P[A]]): StreamT[Id, (A, C)] = {
    val (s, pr) = interpreter.runFree(initialState, p)
    val fetch = this.fetch(pr)
    solutions(s) map { s => (fetch(s), getCost(s)) }
  }

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
        val newStates = sks map { k => interpreter.runFreeUnit(s, k) }
        val heap2 = heap1.insertAllF[List](newStates)
        unfold(heap2)
    }
  }

}
