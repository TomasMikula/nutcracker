package nutcracker.syntax

import nutcracker.Dom

object dom {
  implicit class DomOps[D, U, Δ](d: D)(implicit dom: Dom.Aux[D, U, Δ]) {
    def update(u: U): Option[(D, Δ)] = dom.update(d, u)
    def update_(u: U): D = dom.update_(d, u)
    def assess: Dom.Status[U] = dom.assess(d)
  }
}
