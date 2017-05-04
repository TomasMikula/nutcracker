package nutcracker.toolkit

import nutcracker.util.algebraic.NonDecreasingMonoid
import scalaz._
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.traverse._

class BFSSolver[Prg, S, M[_], C: NonDecreasingMonoid, A](
  interpreter: (Prg, S) => M[S],
  assess: S => List[Prg] \/ A,
  getCost: S => C
)(implicit M: Monad[M]) {

  implicit val orderByCost: Order[S] = Order.orderBy(getCost)

  def solutions(s: S): StreamT[M, (A, C)] = {
    val heap = Heap.singleton(s)
    StreamT.unfoldM(heap)(uncons)
  }

  private def uncons(heap: Heap[S]): M[Option[((A, C), Heap[S])]] = heap.uncons match {
    case Some((s, heap1)) => assess(s) match {
      case -\/(ks) =>
        val newStates = ks traverse { interpreter(_, s) }
        val heap2: M[Heap[S]] = M.map(newStates) { heap1.insertAllF[List](_) }
        heap2 flatMap { uncons(_) }
      case \/-(a) =>
        M.point(Some(((a, getCost(s)), heap1)))
    }
    case None => M.point(None)
  }

}
