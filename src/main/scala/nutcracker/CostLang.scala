package nutcracker

import scala.language.higherKinds

sealed trait CostLang[C, K[_], A]

object CostLang {
  case class Cost[C, K[_]](c: C) extends CostLang[C, K, Unit]
  case class GetCost[C, K[_]]() extends CostLang[C, K, C]

  def cost[C, K[_]](c: C): CostLang[C, K, Unit] = Cost(c)
  def getCost[C, K[_]](): CostLang[C, K, C] = GetCost()
}