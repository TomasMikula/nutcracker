package nutcracker

/** Typeclass for domains that are join-semilattices. */
trait JoinDom[D] extends SyncDom[D] {

  def toJoinUpdate(d: D): Update

  override def toPatch(d: D, δ: Delta): Update = toJoinUpdate(d)

  /** Asymmetric join operation: performs join and returns `Updated` if
    * the result is different from the left argument, `Unchanged` if the
    * result is the same as the left argument.
    */
  def ljoin[D0 <: D](d1: D0, d2: D): UpdateResult[D, IDelta, D0] =
    update(d1, toJoinUpdate(d2))

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

    override def toJoinUpdate(d: D) = d

    override def update[D0 <: D](d: D0, u: Update): UpdateResult[D, IDelta, D0] = ljoin0(d, u) match {
      case Some(d) => UpdateResult(d, ())
      case None    => UpdateResult()
    }

    override def appendDeltas(d1: Unit, d2: Unit): Unit = ()
  }

}

trait SplittableJoinDom[D] extends JoinDom[D] with Splittable[D]

object SplittableJoinDom {
  type Aux[D, U, Δ] = SplittableJoinDom[D] { type Update = U; type Delta = Δ }
}

/** Typeclass for domains that are relatively complemented lattices. */
trait RelativelyComplementedDom[D] extends JoinDom[D] {

  /** Returns update `u` such that for all `d0: D` the result of
    * `update_(d0, u)` is the relative complement of `d` in the
    * sub-lattice `[d0, ⊤]`.
    */
  def toComplementUpdate(d: D): Update

  def exclude[D0 <: D](d1: D0, d2: D): UpdateResult[D, IDelta, D0] =
    update(d1, toComplementUpdate(d2))
}

object RelativelyComplementedDom {
  type Aux[D, U, Δ] = RelativelyComplementedDom[D] { type Update = U; type Delta = Δ }
}