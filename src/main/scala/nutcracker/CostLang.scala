package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK, Lst, Step, WriterState}
import scalaz.{Monoid, ~>}

sealed trait CostLang[C, K[_], A]

object CostLang {
  case class Cost[C, K[_]](c: C) extends CostLang[C, K, Unit]
  case class GetCost[C, K[_]]() extends CostLang[C, K, C]

  final case class CostS[C, K[_]](value: C) extends AnyVal

  def cost[C, K[_]](c: C): CostLang[C, K, Unit] = Cost(c)
  def getCost[C, K[_]](): CostLang[C, K, C] = GetCost()

  implicit def functorKInstance[C]: FunctorKA[CostLang[C, ?[_], ?]] = new FunctorKA[CostLang[C, ?[_], ?]] {

    def transform[K[_], L[_], A](ck: CostLang[C, K, A])(tr: K ~> L): CostLang[C, L, A] = ck match {
      case Cost(c) => cost(c)
      case GetCost() => getCost[C, L]().asInstanceOf[CostLang[C, L, A]] // XXX is there a way to convince scalac that C =:= A?
    }
  }

  def interpreter[C: Monoid]: Step[CostLang[C, ?[_], ?], CostS[C, ?[_]]] =
    new Step[CostLang[C, ?[_], ?], CostS[C, ?[_]]] {
      override def apply[K[_], A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], CostS[C, K], A] = {
        type K1[X] = K[X] // try removing this after this is resolved: https://issues.scala-lang.org/browse/SI-10117
          f match {
          case Cost(c1) => WriterState(c0 => (Lst.empty, CostS[C, K1](Monoid[C].append(c0.value, c1)), ()))
          case GetCost() => WriterState(c0 => (Lst.empty, c0, c0.value.asInstanceOf[A])) // XXX is there a way to convince scalac that C =:= A?
        }
      }
    }

  implicit def costOpsInstance[F[_[_], _], C0](implicit i: InjectK[CostLang[C0, ?[_], ?], F]): CostOps.Aux[FreeK[F, ?], C0] =
    new CostOps[FreeK[F, ?]] {
      type C = C0

      def cost(c: C): FreeK[F, Unit] =
        FreeK.injLiftF(CostLang.cost[C, FreeK[F, ?]](c))

      def getCost: FreeK[F, C] =
        FreeK.injLiftF(CostLang.getCost[C, FreeK[F, ?]]())
    }
}