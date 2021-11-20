package nutcracker.toolkit

import nutcracker.Defer
import nutcracker.util.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{FreeK, Inject, Lst, MonadTellState, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops._
import scalaz.{Bind, Heap, Lens, Order}
import scalaz.syntax.monoid._

private[nutcracker] class DeferModuleImpl[D](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]) extends PersistentDeferModule[D] { self =>
  type Lang[K[_], A] = DeferLang[D, K, A]
  type StateK[K[_]] = DeferStore[D, K]

  implicit def freeDeferApi[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]): Defer[FreeK[F, *], D] =
    new Defer[FreeK[F, *], D] {
      def defer(delay: D, k: FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.liftF(i(DeferLang.defer[D, FreeK[F, *]](delay, k)))
    }

  def emptyK[K[_]] = DeferStore.empty[D, K]

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] =
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

  def isEmpty: Boolean = heap.isEmpty

  def uncons: Option[(DeferStore[D, K], Lst[K[Unit]])] = heap.uncons match {
    case Some(((d, k), heap1)) => Some((DeferStore(d, heap1), Lst.singleton(k)))
    case None => None
  }
}

private[nutcracker] object DeferStore {
  def empty[D, K[_]](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]): DeferStore[D, K] =
    DeferStore(D.zero, Heap.Empty[(D, K[Unit])])

  def interpreter[D, K[_], S](implicit lens: Lens[S, DeferStore[D, K]]): StateInterpreter[K, DeferLang[D, K, *], S] =
    new StateInterpreter[K, DeferLang[D, K, *], S] {
      import DeferLang._

      override def apply[M[_], W, A](fa: DeferLang[D, K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[DeferLang[D, K, *], K], K: Bind[K]): M[A] =
        M.writerState(s => {
          @inline def scheduleExecution: W = Lst.singleton(inj(exec[D, K]())) at 10

          val ds = lens.get(s)
          fa match {
            case Delay(d, k) =>
              val s1 = s set ds.add(d, k)
              if(ds.isEmpty) // was empty, schedule execution
                (scheduleExecution, s1, ())
              else // was non-empty, execution must have been scheduled already
                (W.zero, s1, ())
            case Exec() =>
              ds.uncons match {
                case Some((ds1, ks)) =>
                  if(ds1.isEmpty)
                    (ks at 0, s set ds1, ())
                  else // more tasks to execute => schedule
                    ((ks at 0) |+| scheduleExecution, s set ds1, ())
                case None =>
                  (W.zero, s, ())
              }
          }
        })
    }
}