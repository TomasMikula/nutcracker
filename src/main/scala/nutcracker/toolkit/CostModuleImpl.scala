package nutcracker.toolkit

import nutcracker.CostApi
import nutcracker.util.{FreeK, Inject, Lst, Step, WriterState}
import scalaz.{Lens, Monoid}

private[nutcracker] class CostModuleImpl[C](implicit C: Monoid[C]) extends PersistentCostModule[C] {
  import CostLang._

  type Lang[K[_], A] = CostLang[C, K, A]
  type StateK[K[_]] = C

  override def getCost[K[_]](s: StateK[K]): C = s

  def emptyK[K[_]]: StateK[K] = C.zero

  def stashable = new CostListModule[C, Lang, StateK](this)

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): Step[K, Lang[K, ?], S] = new Step[K, Lang[K, ?], S] {
    override def apply[A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], S, A] =
      go(f).zoomOut

    private def go[A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], StateK[K], A] =
      f match {
        case Cost(c1) => WriterState(c0 => (Lst.empty, C.append(c0, c1), ()))
        case GetCost(ev) => WriterState(c0 => (Lst.empty, c0, ev(c0)))
      }
  }

  implicit def freeCost[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): CostApi.Aux[FreeK[F, ?], C] = {
    type C0 = C
    new CostApi[FreeK[F, ?]] {
      type C = C0

      def cost(c: C): FreeK[F, Unit] =
        FreeK.liftF(i(CostLang.cost[C, FreeK[F, ?]](c)))

      def getCost: FreeK[F, C] =
        FreeK.liftF(i(CostLang.getCost[C, FreeK[F, ?]]()))
    }
  }
}