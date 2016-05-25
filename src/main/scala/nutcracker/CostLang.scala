package nutcracker

import scala.language.higherKinds
import nutcracker.util.{ConstK, FreeK, FunctorKA, Lst, Step, WriterState}

import scalaz.{Monoid, ~>}

sealed trait CostLang[C, K[_], A]

object CostLang {
  case class Cost[C, K[_]](c: C) extends CostLang[C, K, Unit]
  case class GetCost[C, K[_]]() extends CostLang[C, K, C]

  def cost[C, K[_]](c: C): CostLang[C, K, Unit] = Cost(c)
  def getCost[C, K[_]](): CostLang[C, K, C] = GetCost()

  def costF[C](c: C): FreeK[CostLang[C, ?[_], ?], Unit] =
    FreeK.liftF[CostLang[C, ?[_], ?], Unit](cost[C, FreeK[CostLang[C, ?[_], ?], ?]](c))

  implicit def functorKInstance[C]: FunctorKA[CostLang[C, ?[_], ?]] = new FunctorKA[CostLang[C, ?[_], ?]] {

    def transform[K[_], L[_], A](ck: CostLang[C, K, A])(tr: K ~> L): CostLang[C, L, A] = ck match {
      case Cost(c) => cost(c)
      case GetCost() => getCost[C, L]().asInstanceOf[CostLang[C, L, A]] // XXX is there a way to convince scalac that C =:= A?
    }
  }

  def interpreter[C: Monoid]: Step[CostLang[C, ?[_], ?], ConstK[C, ?[_]]] =
    new Step[CostLang[C, ?[_], ?], ConstK[C, ?[_]]] {
      override def apply[K[_], A](f: CostLang[C, K, A]): WriterState[Lst[K[Unit]], ConstK[C, K], A] = f match {
        case Cost(c1) => WriterState(c0 => (Lst.empty, Monoid[C].append(c0, c1), ()))
        case GetCost() => WriterState(c0 => (Lst.empty, c0, c0.asInstanceOf[A])) // XXX is there a way to convince scalac that C =:= A?
      }
    }
}
