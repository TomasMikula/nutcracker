package nutcracker.toolkit

import nutcracker.Defer
import nutcracker.util.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{FreeK, Inject, Lst, StateInterpreter, Step, Uncons, WriterState}
import scalaz.{Heap, Lens, Order}

private[nutcracker] class DeferModuleImpl[D](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]) extends PersistentDeferModule[D] { self =>
  type Lang[K[_], A] = DeferLang[D, K, A]
  type StateK[K[_]] = DeferStore[D, K]

  implicit def freeDeferApi[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): Defer[FreeK[F, ?], D] =
    new Defer[FreeK[F, ?], D] {
      def defer(delay: D, k: FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.liftF(i(DeferLang.defer[D, FreeK[F, ?]](delay, k)))
    }

  def emptyK[K[_]] = DeferStore.empty[D, K]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] =
    DeferStore.interpreter[D, K, S]

  def stashable: StashDeferModule[D] { type Lang[K[_], A] = self.Lang[K, A] } =
    new DeferListModule[D, Lang, StateK](this)
}

private[nutcracker] final case class DeferStore[D, K[_]] private (
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

private[nutcracker] object DeferStore {
  def empty[D, K[_]](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]): DeferStore[D, K] =
    DeferStore(D.zero, Heap.Empty[(D, K[Unit])])

  def interpreter[D, K[_], S](implicit lens: Lens[S, DeferStore[D, K]]): StateInterpreter[K, DeferLang[D, K, ?], S] =
    new StateInterpreter[K, DeferLang[D, K, ?], S] {
      import DeferLang._

      def step: Step[K, DeferLang[D, K, ?], S] = new Step[K, DeferLang[D, K, ?], S] {
        def apply[A](f: DeferLang[D, K, A]): WriterState[Lst[K[Unit]], S, A] =
          WriterState(s => f match {
            case Delay(d, k) => (Lst.empty, lens.mod(_.add(d, k), s), ())
          })
      }

      def uncons: Uncons[K, S] = Uncons[K, DeferStore[D, K]](_.uncons).zoomOut[S]
    }
}