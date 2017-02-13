package nutcracker

/** Typeclass for domains that are join-semilattices. */
trait JoinDom[D] extends Dom[D] {

  /** Asymmetric join operation: performs join and returns `Some` if
    * the result is different from the left argument, `None` if the
    * result is the same as the left argument.
    */
  def ljoin(d1: D, d2: D): Option[(D, Delta)]

  /** Join operation (as in join-semilattice). */
  def join(d1: D, d2: D): D = ljoin(d1, d2).fold(d1)(_._1)
}

object JoinDom {
  type Aux[D, U, Δ] = JoinDom[D] { type Update = U; type Delta = Δ }

  trait Template[D] extends JoinDom[D] {

    /** Update is a join with a value of the same type. */
    type Update = D

    /** We don't generally have a useful description of the delta. */
    type Delta = Unit

    def ljoin0(d1: D, d2: D): Option[D]

    override def ljoin(d1: D, d2: D): Option[(D, Delta)] = ljoin0(d1, d2).map((_, ()))
    override def update(d: D, u: Update): Option[(D, Delta)] = ljoin(d, u)
    override def appendDeltas(d1: Unit, d2: Unit): Unit = ()
  }

}