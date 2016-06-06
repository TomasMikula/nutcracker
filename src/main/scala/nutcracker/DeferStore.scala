package nutcracker

import scala.language.higherKinds
import nutcracker.DeferLang.Defer
import nutcracker.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{Lst, StateInterpreter, Step, Uncons, ValK, WriterState}

import scalaz.{Heap, Order, StateT}
import scalaz.std.option._

final case class DeferStore[D, K[_]] private (
  private val currentTime: D,
  private val heap: Heap[(D, K[Unit])]
)(implicit
  D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]
) {
  private implicit val ord: Order[(D, K[Unit])] = Order.orderBy(_._1)

  def +(d: Defer[D, K]): DeferStore[D, K] = {
    copy(heap = heap + ((D.append(currentTime, d.delay), d.k)))
  }

  def uncons: Option[(DeferStore[D, K], Lst[K[Unit]])] = heap.uncons match {
    case Some(((d, k), heap1)) => Some((DeferStore(d, heap1), Lst.singleton(k)))
    case None => None
  }
}

object DeferStore {
  def empty[D, K[_]](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]) =
    DeferStore(D.zero, Heap.Empty[(D, K[Unit])])

  def interpreter[D]: StateInterpreter.Aux[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] =
    new StateInterpreter[DeferLang[D, ?[_], ?]] {
      type State[K[_]] = DeferStore[D, K]

      def step: Step[DeferLang[D, ?[_], ?], State] = new Step[DeferLang[D, ?[_], ?], State] {
        def apply[K[_], A](f: DeferLang[D, K, A]): WriterState[Lst[K[Unit]], State[K], A] =
          WriterState(s => f match {
            case d @ Defer(_, _) => (Lst.empty, s + d, ())
          })
      }

      def uncons: Uncons[State] = Uncons(new ValK[λ[K[_] => StateT[Option, State[K], Lst[K[Unit]]]]] {
        protected def compute[K[_]]: StateT[Option, State[K], Lst[K[Unit]]] =
          StateT(_.uncons)
      })
    }
}