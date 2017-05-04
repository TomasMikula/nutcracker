package nutcracker.toolkit

import scala.language.higherKinds
import scalaz.Leibniz
import scalaz.Leibniz.===

sealed trait CostLang[C, K[_], A]

object CostLang {
  case class Cost[C, K[_]](c: C) extends CostLang[C, K, Unit]
  case class GetCost[C, K[_], A](ev: C === A) extends CostLang[C, K, A]

  def cost[C, K[_]](c: C): CostLang[C, K, Unit] = Cost(c)
  def getCost[C, K[_]](): CostLang[C, K, C] = GetCost(Leibniz.refl[C])
}