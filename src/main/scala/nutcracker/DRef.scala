package nutcracker

import nutcracker.util.EqualK

import scalaz.{Equal, Show}

sealed abstract class DRef[D](private[nutcracker] val domainId: Long) {
  type Update
  type Delta

  /** Infer `Update` and `Delta` types. Relies on global uniqueness
    * of `Dom[D]` instances.
    */
  def infer(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
    this.asInstanceOf[DRef.Aux[D, dom.Update, dom.Delta]]
}

object DRef {
  type Aux[D, U, Δ] = DRef[D] { type Update = U; type Delta = Δ }

  def apply[D](domainId: Long)(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
    new DRef[D](domainId) {
      type Update = dom.Update
      type Delta = dom.Delta
    }

  implicit def equalInstance[D, U, Δ]: Equal[DRef.Aux[D, U, Δ]] = new Equal[DRef.Aux[D, U, Δ]] {
    def equal(r1: Aux[D, U, Δ], r2: Aux[D, U, Δ]): Boolean = r1.domainId == r2.domainId
  }

  implicit def equalKInstance[D]: EqualK[DRef] = new EqualK[DRef] {
    def equal[A](f1: DRef[A], f2: DRef[A]): Boolean = f1.domainId == f2.domainId
  }

  implicit def showInstance[D, U, Δ]: Show[DRef.Aux[D, U, Δ]] = new Show[DRef.Aux[D, U, Δ]] {
    override def shows(ref: DRef.Aux[D, U, Δ]): String = s"ref${ref.domainId}"
  }
}
