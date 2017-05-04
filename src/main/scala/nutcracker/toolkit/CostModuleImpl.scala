package nutcracker.toolkit

import nutcracker.CostApi
import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import scalaz.{Monad, Monoid}

private[nutcracker] class CostModuleImpl[C](implicit C: Monoid[C]) extends PersistentCostModule[C] {
  import CostLang._

  type Lang[K[_], A] = CostLang[C, K, A]
  type StateK[K[_]] = C

  override def getCost[K[_]](s: StateK[K]): C = s

  def emptyK[K[_]]: StateK[K] = C.zero

  def stashable = new CostListModule[C, Lang, StateK](this)

  val interpreter: Step[Lang, StateK] =
    new Step[Lang, StateK] {
      override def apply[K[_]: Monad, A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], StateK[K], A] = {
        f match {
          case Cost(c1) => WriterState(c0 => (Lst.empty, C.append(c0, c1), ()))
          case GetCost(ev) => WriterState(c0 => (Lst.empty, c0, ev(c0)))
        }
      }
    }

  implicit def freeCost[F[_[_], _]](implicit i: InjectK[Lang, F]): CostApi.Aux[FreeK[F, ?], C] = {
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