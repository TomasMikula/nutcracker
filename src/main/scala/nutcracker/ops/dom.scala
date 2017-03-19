package nutcracker.ops

import nutcracker.{Dom, UpdateResult}
import scala.language.implicitConversions

object dom extends ToDomOps

trait ToDomOps {
  implicit def domOps[D](d: D)(implicit dom: Dom[D]): DomOps[D, dom.Update, dom.Delta] =
    new DomOps(d)(dom)
}

final class DomOps[D, U, Δ](d: D)(val dom: Dom.Aux[D, U, Δ]) {
  def update(u: U): UpdateResult[D, dom.IDelta, D] = dom.update(d, u)
  def update_(u: U): D = dom.update_(d, u)
  def isFailed: Boolean = dom.isFailed(d)
}