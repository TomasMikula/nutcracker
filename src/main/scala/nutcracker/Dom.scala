package nutcracker

import scalaz.{Leibniz, Semigroup, \&/, ===}
import scalaz.Isomorphism.<=>

trait IDom[D[_]] {
  type IUpdate[_]
  type IDelta[_, _]

  /** Applies the monotonic update `u` to `d`, obtaining `d1 ≥ d`
    * and a description of the diff. If the update doesn't have any
    * effect on `d`, returns `Unchanged`. In addition to being monotonic,
    * updates also have to be _commutative_ and _idempotent_ (when
    * the deltas are combined using [[composeDeltas]]).
    */
  def iUpdate[I, J](d: D[I], u: IUpdate[J]): IUpdateResult[D, IDelta, I, J]

  def composeDeltas[D1, D2, D3](δ1: IDelta[D2, D3], δ2: IDelta[D1, D2]): IDelta[D1, D3]

  def iIsFailed[I](d: D[I]): Boolean

  final def aux: IDom.Aux[D, IUpdate, IDelta] = this
}

object IDom {
  type Aux[D[_], U[_], Δ[_, _]] = IDom[D] { type IUpdate[I] = U[I]; type IDelta[D1, D2] = Δ[D1, D2] }
}

sealed abstract class IUpdateResult[D[_], Δ[_, _], I, J] {
  def map[E[_], Δ2[_, _]](f: [i] => D[i] => E[i], g: [i, j] => Δ[i, j] => Δ2[i, j]): IUpdateResult[E, Δ2, I, J]

  def map_[E, Δ2](f: [i] => D[i] => E, g: [i, j] => Δ[i, j] => Δ2): UpdateResult[E, Δ2] =
    UpdateResult(map[[i] =>> E, [i, j] =>> Δ2](f, g))
}
case class Unchanged[D[_], Δ[_, _], I]() extends IUpdateResult[D, Δ, I, I] {
  override def map[E[_], Δ2[_, _]](f: [i] => D[i] => E[i], g: [i, j] => Δ[i, j] => Δ2[i, j]): IUpdateResult[E, Δ2, I, I] =
    IUpdateResult.unchanged
}
case class Updated[D[_], Δ[_, _], I, J](newValue: D[J], delta: Δ[I, J]) extends IUpdateResult[D, Δ, I, J] {
  override def map[E[_], Δ2[_, _]](f: [i] => D[i] => E[i], g: [i, j] => Δ[i, j] => Δ2[i, j]): IUpdateResult[E, Δ2, I, J] =
    Updated(f(newValue), g(delta))
}

object IUpdateResult {
  private val _unchanged = Unchanged()

  def unchanged[D[_], Δ[_, _], I]: IUpdateResult[D, Δ, I, I] =
    _unchanged.asInstanceOf[IUpdateResult[D, Δ, I, I]]

  def updated[D[_], Δ[_, _], I, J](newVal: D[J], δ: Δ[I, J]): IUpdateResult[D, Δ, I, J] =
    Updated(newVal, δ)
}

trait Dom[D] extends IDom[[i] =>> D] {
  type Domain = D
  type Update
  type Delta

  type IUpdate[I] = Update
  type IDelta[I, J] = Delta

  def update(d: D, u: Update): UpdateResult[D, Delta]

  override def iUpdate[I, J](d: D, u: Update): IUpdateResult[[i] =>> D, IDelta, I, J] =
    update(d, u).at[I, J]

  /** Associative, idempotent and monotonic (non-decreasing) operation
    * to combine diffs.
    */
  def appendDeltas(d1: Delta, d2: Delta): Delta

  final override def composeDeltas[D1, D2, D3](δ1: IDelta[D2, D3], δ2: IDelta[D1, D2]): IDelta[D1, D3] =
    appendDeltas(δ2, δ1)

  def isFailed(d: D): Boolean

  override def iIsFailed[I](d: D): Boolean =
    isFailed(d)

  /** A variation on [[update]] that always returns the updated value,
    * whether changed or unchaged. */
  def update_(d: D, u: Update): D = update(d, u).newValueOr(d)

  def deltaSemigroup: Semigroup[Delta] = new Semigroup[Delta] {
    def append(d1: Delta, d2: => Delta): Delta = appendDeltas(d1, d2)
  }
}

object Dom {
  type Aux[D, U, Δ] = Dom[D] { type Update = U; type Delta = Δ }

  def apply[D](implicit dom: Dom[D]): Dom.Aux[D, dom.Update, dom.Delta] = dom

  def relateUpdates[D](d1: Dom[D], d2: Dom[D]): d1.Update === d2.Update = // linter:ignore UnusedParameter
    Leibniz.force[Nothing, Any, d1.Update, d2.Update]

  def relateDeltas[D](d1: Dom[D], d2: Dom[D]): d1.Delta === d2.Delta = // linter:ignore UnusedParameter
    Leibniz.force[Nothing, Any, d1.Delta, d2.Delta]

  def via[A, B](iso: A <=> B)(implicit domB: Dom[B]): Dom.Aux[A, domB.Update, domB.Delta] =
    new Dom[A] {
      type Update = domB.Update
      type Delta = domB.Delta

      def update(a: A, u: Update): UpdateResult[A, Delta] =
        domB.update(iso.to(a), u).mapDomain(iso.from)

      def appendDeltas(d1: Delta, d2: Delta): Delta =
        domB.appendDeltas(d1, d2)

      def isFailed(a: A): Boolean =
        domB.isFailed(iso.to(a))
    }

  def tuple2[D1, D2](implicit dom1: Dom[D1], dom2: Dom[D2]): Dom.Aux[(D1, D2), dom1.Update \&/ dom2.Update, dom1.Delta \&/ dom2.Delta] =
    new Dom[(D1, D2)] {
      type Update = dom1.Update \&/ dom2.Update
      type Delta = dom1.Delta \&/ dom2.Delta

      def update(d: (D1, D2), u: Update): UpdateResult[(D1, D2), Delta] = u match {
        case \&/.This(u1) =>
          dom1.update(d._1, u1).map((_, d._2), \&/.This(_))
        case \&/.That(u2) =>
          dom2.update(d._2, u2).map((d._1, _), \&/.That(_))
        case \&/.Both(u1, u2) =>
          (dom1.update(d._1, u1).at[Any, Any], dom2.update(d._2, u2).at[Any, Any]) match {
            case (Updated(d1, δ1), Updated(d2, δ2)) => UpdateResult.updated((d1, d2  ), \&/.Both(δ1, δ2))
            case (Updated(d1, δ1), Unchanged()    ) => UpdateResult.updated((d1, d._2), \&/.This(δ1))
            case (Unchanged()    , Updated(d2, δ2)) => UpdateResult.updated((d._1, d2), \&/.That(δ2))
            case (Unchanged()    , Unchanged()    ) => UpdateResult.unchanged
          }
      }

      def appendDeltas(δ1: Delta, δ2: Delta): Delta =
        deltaSemigroup.append(δ1, δ2)

      def isFailed(d: (D1, D2)): Boolean =
        dom1.isFailed(d._1) || dom2.isFailed(d._2)

      override val deltaSemigroup = \&/.TheseSemigroup(dom1.deltaSemigroup, dom2.deltaSemigroup)
    }

  /** import content to get implicit tupled Doms */
  object tupled {
    implicit def tuple2[D1, D2](implicit
      dom1: Dom[D1],
      dom2: Dom[D2],
    ): Dom.Aux[(D1, D2), dom1.Update \&/ dom2.Update, dom1.Delta \&/ dom2.Delta] =
      Dom.tuple2[D1, D2]
  }
}