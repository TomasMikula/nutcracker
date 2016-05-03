package nutcracker

import nutcracker.Assessment._
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.FreeK

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz._
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.traverse._

class BFSSolver[F[_[_], _], St[_[_]], M[_], P[_], C: NonDecreasingMonoid](
  interpreter: FreeK[F, ?] ~> scalaz.StateT[M, St[FreeK[F, ?]], ?],
  initialState: St[FreeK[F, ?]],
  assess: St[FreeK[F, ?]] => Assessment[List[FreeK[F, Unit]]],
  fetch: P ~> (St[FreeK[F, ?]] => ?),
  getCost: St[FreeK[F, ?]] => C
)(implicit M: Monad[M]) {
  val lang: PropRelCost[C] = new PropRelCost[C]

  type K[A] = FreeK[F, A]
  type S = St[K]

  implicit val orderByCost: Order[S] = Order.orderBy(getCost)

  def solutions[A](p: K[P[A]]): StreamT[M, (A, C)] = {
    StreamT.wrapEffect(M.map(interpreter(p)(initialState)) { case (s, pr) =>
      val fetch = this.fetch(pr)
      solutions(s) map { s => (fetch(s), getCost(s)) }
    })
  }

  private def solutions(s: S): StreamT[M, S] = {
    val heap = Heap.singleton[S](s)
    StreamT.unfoldM(heap)(unfold)
  }

  @tailrec
  private def unfold(heap: Heap[S]): M[Option[(S, Heap[S])]] = heap.uncons match {
    case None => M.point(None)
    case Some((s, heap1)) => assess(s) match {
      case Failed => unfold(heap1)
      case Stuck => unfold(heap1) // not done, but we don't know how to proceed. TODO: Don't treat as failed
      case Done => M.point(Some((s, heap1)))
      case Incomplete(sks) =>
        val newStates = sks traverse { k => interpreter(k).exec(s) }
        val heap2: M[Heap[S]] = M.map(newStates) { heap1.insertAllF[List](_) }
        heap2 flatMap { unfold1(_) }
    }
  }

  // Alias for unfold that can be invoked in a non-tail position from otherwise
  // tail-recursive unfold.
  private def unfold1(heap: Heap[S]): M[Option[(S, Heap[S])]] = unfold(heap)

}
