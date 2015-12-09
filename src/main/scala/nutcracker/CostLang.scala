package nutcracker

import scala.language.higherKinds

import nutcracker.util.free.Interpreter
import nutcracker.util.free.Interpreter.{ConstK, CleanInterpreter, AlwaysClean}

import scalaz.{Semigroup, Applicative}
import scalaz.syntax.applicative._

sealed trait CostLang[C, K[_], A]

object CostLang {
  case class Cost[C, K[_]](c: C) extends CostLang[C, K, Unit]
  case class GetCost[C, K[_]]() extends CostLang[C, K, C]

  implicit def interpreter[C: Semigroup]: Interpreter[CostLang[C, ?[_], ?], ConstK[C, ?[_]], AlwaysClean] =
    new CleanInterpreter[CostLang[C, ?[_], ?], ConstK[C, ?[_]]] {

      def step0[K[_] : Applicative, A](f: CostLang[C, K, A])(c0: C): (ConstK[C, K], K[A]) = f match {
        case Cost(c1) => (Semigroup[C].append(c0, c1), ().pure[K])
        case GetCost() => (c0, c0.asInstanceOf[A].pure[K]) // XXX is there a way to convince scalac that C =:= A?
      }

    }
}