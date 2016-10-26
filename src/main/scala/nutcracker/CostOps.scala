package nutcracker

import scala.language.higherKinds

trait CostOps[M[_]] {
  type C

  def cost(c: C): M[Unit]
  def getCost: M[C]
}

object CostOps {
  type Aux[M[_], C0] = CostOps[M] { type C = C0 }

  def apply[M[_]](implicit ev: CostOps[M]): CostOps.Aux[M, ev.C] = ev
}