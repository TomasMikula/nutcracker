package nutcracker

import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import scalaz.Monoid

private[nutcracker] class CostModuleImpl[C](implicit C: Monoid[C]) extends PersistentCostModule[C] {
  import CostLang._

  type Lang[K[_], A] = CostLang[C, K, A]
  type State[K[_]] = CostS[C, K]

  override def getCost[K[_]](s: State[K]): C = s.value

  def empty[K[_]]: State[K] = CostS(C.zero)

  def stashable = new CostListModule[C, Lang, State](this)

  val interpreter: Step[Lang, State] =
    new Step[CostLang[C, ?[_], ?], CostS[C, ?[_]]] {
      override def apply[K[_], A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], CostS[C, K], A] = {
        type K1[X] = K[X] // try removing this after this is resolved: https://issues.scala-lang.org/browse/SI-10117
        f match {
          case Cost(c1) => WriterState(c0 => (Lst.empty, CostS[C, K1](C.append(c0.value, c1)), ()))
          case GetCost() => WriterState(c0 => (Lst.empty, c0, c0.value.asInstanceOf[A])) // XXX is there a way to convince scalac that C =:= A?
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

private[nutcracker] final case class CostS[C, K[_]](value: C) // extends AnyVal