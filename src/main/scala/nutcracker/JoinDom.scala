package nutcracker

/** Typeclass for domains that are join-semilattices. */
trait JoinDom[D] extends Dom[D] {

  /** Asymmetric join operation: performs join and returns `Updated` if
    * the result is different from the left argument, `Unchanged` if the
    * result is the same as the left argument.
    */
  def ljoin[D0 <: D](d1: D0, d2: D): UpdateResult[D, IDelta, D0]

  /** Join operation (as in join-semilattice). */
  def join(d1: D, d2: D): D = ljoin(d1, d2) match {
    case Updated(d, _) => d
    case Unchanged()   => d1
  }
}

object JoinDom {
  type Aux[D, U, Δ] = JoinDom[D] { type Update = U; type Delta = Δ }

  trait Template[D] extends JoinDom[D] {

    /** Update is a join with a value of the same type. */
    type Update = D

    /** We don't generally have a useful description of the delta. */
    type Delta = Unit

    def ljoin0(d1: D, d2: D): Option[D]

    override def ljoin[D0 <: D](d1: D0, d2: D): UpdateResult[D, IDelta, D0] = ljoin0(d1, d2) match {
      case Some(d) => UpdateResult(d, ())
      case None    => UpdateResult()
    }
    override def update[D0 <: D](d: D0, u: Update): UpdateResult[D, IDelta, D0] = ljoin(d, u)
    override def appendDeltas(d1: Unit, d2: Unit): Unit = ()
  }

}