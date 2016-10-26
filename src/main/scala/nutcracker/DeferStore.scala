package nutcracker

import scala.language.higherKinds
import nutcracker.DeferLang.Delay
import nutcracker.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{Lst, StateInterpreter, Step, Uncons, ValA, WriterState}

import scalaz.{Heap, Order, StateT}
import scalaz.std.option._

final case class DeferStore[D, K] private (
  private val currentTime: D,
  private val heap: Heap[(D, K)]
)(implicit
  D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]
) {
  private implicit val ord: Order[(D, K)] = Order.orderBy(_._1)

  def add(d: D, k: K): DeferStore[D, K] = {
    copy(heap = heap + ((D.append(currentTime, d), k)))
  }

  def uncons: Option[(DeferStore[D, K], Lst[K])] = heap.uncons match {
    case Some(((d, k), heap1)) => Some((DeferStore(d, heap1), Lst.singleton(k)))
    case None => None
  }
}

object DeferStore {
  def empty[D, K](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]): DeferStore[D, K] =
    DeferStore(D.zero, Heap.Empty[(D, K)])

  def interpreter[D]: StateInterpreter[DeferLang[D, ?[_], ?], DeferStore[D, ?]] =
    new StateInterpreter[DeferLang[D, ?[_], ?], DeferStore[D, ?]] {

      def step: Step[DeferLang[D, ?[_], ?], DeferStore[D, ?]] = new Step[DeferLang[D, ?[_], ?], DeferStore[D, ?]] {
        def apply[K[_], A](f: DeferLang[D, K, A]): WriterState[Lst[K[Unit]], DeferStore[D, K[Unit]], A] =
          WriterState(s => f match {
            case Delay(d, k) => (Lst.empty, s.add(d, k), ())
          })
      }

      def uncons: Uncons[DeferStore[D, ?]] = Uncons(new ValA[λ[K => StateT[Option, DeferStore[D, K], Lst[K]]]] {
        protected def compute[K]: StateT[Option, DeferStore[D, K], Lst[K]] =
          StateT(_.uncons)
      })
    }
}
