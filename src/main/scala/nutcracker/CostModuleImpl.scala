package nutcracker

import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import scalaz.{Monad, Monoid}

private[nutcracker] class CostModuleImpl[C](implicit C: Monoid[C]) extends PersistentCostModule[C] {
  import CostLang._

  type Lang[K[_], A] = CostLang[C, K, A]
  type State[K[_]] = C

  override def getCost[K[_]](s: State[K]): C = s

  def empty[K[_]]: State[K] = C.zero

  def stashable = new CostListModule[C, Lang, State](this)

  val interpreter: Step[Lang, State] =
    new Step[Lang, State] {
      override def apply[K[_]: Monad, A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], State[K], A] = {
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
        FreeK.injLiftF(CostLang.cost[C, FreeK[F, ?]](c))

      def getCost: FreeK[F, C] =
        FreeK.injLiftF(CostLang.getCost[C, FreeK[F, ?]]())
    }
  }
}