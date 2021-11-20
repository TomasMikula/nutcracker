package nutcracker.toolkit

import nutcracker.CostApi
import nutcracker.util.{FreeK, Inject, Lst, MonadTellState, StateInterpreter, StratifiedMonoidAggregator}
import scalaz.{Bind, Lens, Monoid}

private[nutcracker] class CostModuleImpl[C](implicit C: Monoid[C]) extends PersistentCostModule[C] {
  import CostLang._

  type Lang[K[_], A] = CostLang[C, K, A]
  type StateK[K[_]] = C

  override def getCost[K[_]](s: StateK[K]): C = s

  def emptyK[K[_]]: StateK[K] = C.zero

  def stashable = new CostListModule[C, Lang, StateK](this)

  def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] = new StateInterpreter[K, Lang[K, *], S] {
    def apply[M[_], W, A](fa: CostLang[C, K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[CostLang[C, K, *], K], K: Bind[K]): M[A] =
      fa match {
        case Cost(c1) => M.writerState(s => (W.zero, lens.mod(C.append(_, c1), s), ()))
        case GetCost(ev) => M.writerState(s => (W.zero, s, ev(lens.get(s))))
      }
  }

  implicit def freeCost[F[_[_], _]](implicit i: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]): CostApi.Aux[FreeK[F, *], C] = {
    type C0 = C
    new CostApi[FreeK[F, *]] {
      type C = C0

      def cost(c: C): FreeK[F, Unit] =
        FreeK.liftF(i(CostLang.cost[C, FreeK[F, *]](c)))

      def getCost: FreeK[F, C] =
        FreeK.liftF(i(CostLang.getCost[C, FreeK[F, *]]()))
    }
  }
}