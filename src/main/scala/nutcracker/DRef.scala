package nutcracker

import nutcracker.util.{HEqualK, ShowK}

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

  private[nutcracker] def apply[D](domainId: Long)(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
    new DRef[D](domainId) {
      type Update = dom.Update
      type Delta = dom.Delta
    }

  implicit def equalInstance[D]: Equal[DRef[D]] = new Equal[DRef[D]] {
    def equal(r1: DRef[D], r2: DRef[D]): Boolean = r1.domainId == r2.domainId
  }

  implicit val equalKInstance: HEqualK[DRef] = new HEqualK[DRef] {
    def hEqual[A, B](f1: DRef[A], f2: DRef[B]): Boolean = f1.domainId == f2.domainId
  }

  implicit def showInstance[D]: Show[DRef[D]] = new Show[DRef[D]] {
    override def shows(ref: DRef[D]): String = s"ref${ref.domainId}"
  }

  implicit def showKInstance: ShowK[DRef] = new ShowK[DRef] {
    def shows[A](ref: DRef[A]): String = s"ref${ref.domainId}"
  }
}
