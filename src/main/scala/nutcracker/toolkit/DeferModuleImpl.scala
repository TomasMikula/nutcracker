package nutcracker.toolkit

import nutcracker.Defer
import nutcracker.util.algebraic.{NonDecreasingMonoid, OrderPreservingMonoid}
import nutcracker.util.{FreeK, InjectK, Lst, StateInterpreter, Step, Uncons, WriterState, `Forall{(* -> *) -> *}`}
import scalaz.std.option._
import scalaz.{Heap, Monad, Order, StateT}

private[nutcracker] class DeferModuleImpl[D](implicit D: NonDecreasingMonoid[D] with OrderPreservingMonoid[D]) extends PersistentDeferModule[D] { self =>
  type Lang[K[_], A] = DeferLang[D, K, A]
  type StateK[K[_]] = DeferStore[D, K]

  implicit def freeDeferApi[F[_[_], _]](implicit i: InjectK[Lang, F]): Defer[FreeK[F, ?], D] =
    new Defer[FreeK[F, ?], D] {
      def defer(delay: D, k: FreeK[F, Unit]): FreeK[F, Unit] =
        FreeK.liftF(i(DeferLang.defer[D, FreeK[F, ?]](delay, k)))
    }

  def emptyK[K[_]] = DeferStore.empty[D, K]

  def interpreter: StateInterpreter[Lang, StateK] = DeferStore.interpreter[D]

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

  def interpreter[D]: StateInterpreter[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] =
    new StateInterpreter[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] {
      import DeferLang._

      def step: Step[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] = new Step[DeferLang[D, ?[_], ?], DeferStore[D, ?[_]]] {
        def apply[K[_]: Monad, A](f: DeferLang[D, K, A]): WriterState[Lst[K[Unit]], DeferStore[D, K], A] =
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