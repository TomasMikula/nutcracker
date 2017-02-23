package nutcracker.syntax

import scala.language.implicitConversions
import nutcracker.{Dom, UpdateResult}

object dom extends DomSyntax

trait DomSyntax {
  implicit def domOps[D](d: D)(implicit dom: Dom[D]): DomSyntax.Ops[D, dom.Update, dom.Delta] =
    new DomSyntax.Ops(d)(dom)
}

object DomSyntax {
  class Ops[D, U, Δ](d: D)(val dom: Dom.Aux[D, U, Δ]) {
    def update(u: U): UpdateResult[D, dom.IDelta, D] = dom.update(d, u)
    def update_(u: U): D = dom.update_(d, u)
    def isFailed: Boolean = dom.isFailed(d)
  }
}