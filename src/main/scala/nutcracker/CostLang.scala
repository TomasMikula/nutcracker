package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.{ConstK, FreeK, FunctorKA, Interpreter}
import nutcracker.util.free.Interpreter.CleanInterpreter

import scalaz.{~>, Monoid, Applicative}
import scalaz.syntax.applicative._

sealed trait CostLang[C, K[_], A]

object CostLang {
  case class Cost[C, K[_]](c: C) extends CostLang[C, K, Unit]
  case class GetCost[C, K[_]]() extends CostLang[C, K, C]

  def cost[C, K[_]](c: C): CostLang[C, K, Unit] = Cost(c)
  def getCost[C, K[_]](): CostLang[C, K, C] = GetCost()

  def costF[C](c: C): FreeK[CostLang[C, ?[_], ?], Unit] =
    FreeK.suspend[CostLang[C, ?[_], ?], Unit](cost[C, FreeK[CostLang[C, ?[_], ?], ?]](c))

  implicit def functorKInstance[C]: FunctorKA[CostLang[C, ?[_], ?]] = new FunctorKA[CostLang[C, ?[_], ?]] {

    def transform[K[_], L[_], A](ck: CostLang[C, K, A])(tr: K ~> L): CostLang[C, L, A] = ck match {
      case Cost(c) => cost(c)
      case GetCost() => getCost[C, L]().asInstanceOf[CostLang[C, L, A]] // XXX is there a way to convince scalac that C =:= A?
    }
  }

  implicit def interpreter[C: Monoid]: Interpreter.Aux[CostLang[C, ?[_], ?], ConstK[C, ?[_]]] =
    new CleanInterpreter[CostLang[C, ?[_], ?], ConstK[C, ?[_]]] {

      def step[K[_] : Applicative]: CostLang[C, K, ?] ~> λ[A => scalaz.State[C, K[A]]] = new (CostLang[C, K, ?] ~> λ[A => scalaz.State[C, K[A]]]) {
        override def apply[A](f: CostLang[C, K, A]): scalaz.State[C, K[A]] = f match {
          case Cost(c1) => scalaz.State(c0 => (Monoid[C].append(c0, c1), ().point[K]))
          case GetCost() => scalaz.State(c0 => (c0, c0.asInstanceOf[A].point[K])) // XXX is there a way to convince scalac that C =:= A?
        }
      }

    }
}