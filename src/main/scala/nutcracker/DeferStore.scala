package nutcracker

import scala.language.higherKinds
import nutcracker.DeferLang.Delay
import nutcracker.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{Lst, StateInterpreter, Step, Uncons, `Forall{(* -> *) -> *}`, WriterState}

import scalaz.{Heap, Order, StateT}
import scalaz.std.option._

final case class DeferStore[D, K[_]] private (
  private val currentTime: D,
  private val heap: Heap[(D, K[Unit])]
)(implicit
  D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]
) {
  private implicit val ord: Order[(D, K[Unit])] = Order.orderBy(_._1)

  def add(d: D, k: K[Unit]): DeferStore[D, K] = {
    copy(heap = heap + ((D.append(currentTime, d), k)))
  }

  def uncons: Option[(DeferStore[D, K], Lst[K[Unit]])] = heap.uncons match {
    case Some(((d, k), heap1)) => Some((DeferStore(d, heap1), Lst.singleton(k)))
    case None => None
  }
}

object DeferStore {
  def empty[D, K[_]](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]): DeferStore[D, K] =
    DeferStore(D.zero, Heap.Empty[(D, K[Unit])])

  def interpreter[D]: StateInterpreter[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] =
    new StateInterpreter[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] {

      def step: Step[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] = new Step[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] {
        def apply[K[_], A](f: DeferLang[D, K, A]): WriterState[Lst[K[Unit]], DeferStore[D, K], A] =
          WriterState(s => f match {
            case Delay(d, k) => (Lst.empty, s.add(d, k), ())
          })
      }

      def uncons: Uncons[DeferStore[D, ?[_]]] = Uncons(new `Forall{(* -> *) -> *}`[λ[K[_] => StateT[Option, DeferStore[D, K], Lst[K[Unit]]]]] {
        protected def compute[K[_]]: StateT[Option, DeferStore[D, K], Lst[K[Unit]]] =
          StateT(_.uncons)
      })
    }
}
