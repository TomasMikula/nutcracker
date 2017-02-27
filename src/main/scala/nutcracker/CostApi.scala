package nutcracker

import scala.language.higherKinds

trait CostApi[M[_]] {
  type C

  def cost(c: C): M[Unit]
  def getCost: M[C]
}

object CostApi {
  type Aux[M[_], C0] = CostApi[M] { type C = C0 }

  def apply[M[_]](implicit ev: CostApi[M]): CostApi.Aux[M, ev.C] = ev
}