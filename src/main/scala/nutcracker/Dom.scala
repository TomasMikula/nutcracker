package nutcracker

import scalaz.{Leibniz, Semigroup, \&/}
import scalaz.Isomorphism.<=>
import scalaz.Leibniz.===

trait IDom[D] {
  type Domain = D
  type Update
  type IDelta[_, _]

  /** Applies the monotonic update `u` to `d`, obtaining `d1 ≥ d`
    * and a description of the diff. If the update doesn't have any
    * effect on `d`, returns `Unchanged`. In addition to being monotonic,
    * updates also have to be _commutative_ and _idempotent_ (when
    * the deltas are combined using [[composeDeltas()]]).
    */
  def update[D0 <: D](d: D0, u: Update): UpdateResult[D, IDelta, D0]

  def composeDeltas[D1, D2, D3](δ1: IDelta[D2, D3], δ2: IDelta[D1, D2]): IDelta[D1, D3]

  def isFailed(d: D): Boolean

  final def aux: IDom.Aux[D, Update, IDelta] = this
}

object IDom {
  type Aux[D, U, Δ[_, _]] = IDom[D] { type Update = U; type IDelta[D1, D2] = Δ[D1, D2] }
}

sealed abstract class UpdateResult[D, Δ[_, _], D0] {
  type NewValue

  def map[E, Δ2, E0 <: E](f: D => E, g: Δ[D0, NewValue] => Δ2): UpdateResult[E, λ[(α, β) => Δ2], E0]
}
case class Unchanged[D, Δ[_, _], D0]() extends UpdateResult[D, Δ, D0] {
  type NewValue = Nothing
  def map[E, Δ2, E0 <: E](f: D => E, g: Δ[D0, NewValue] => Δ2): UpdateResult[E, λ[(α, β) => Δ2], E0] =
    UpdateResult()
}
case class Updated[D, Δ[_, _], D0, D1 <: D](newValue: D1, delta: Δ[D0, D1]) extends UpdateResult[D, Δ, D0] {
  type NewValue = D1
  def map[E, Δ2, E0 <: E](f: D => E, g: Δ[D0, NewValue] => Δ2): UpdateResult[E, λ[(α, β) => Δ2], E0] =
    UpdateResult(f(newValue), g(delta))
}

object UpdateResult {
  private val _unchanged = Unchanged() // linter:ignore UndesirableTypeInference

  def unchanged[D, Δ[_, _], D0](): UpdateResult[D, Δ, D0] =
    _unchanged.asInstanceOf[UpdateResult[D, Δ, D0]]

  def updated[D, Δ[_, _], D0, D1 <: D](newVal: D1, δ: Δ[D0, D1]): UpdateResult[D, Δ, D0] =
    Updated(newVal, δ)

  def apply[D, Δ, D0](): UpdateResult[D, λ[(α, β) => Δ], D0] =
    unchanged[D, λ[(α, β) => Δ], D0]

  def apply[D, Δ, D0](newValue: D, δ: Δ): UpdateResult[D, λ[(α, β) => Δ], D0] =
    updated[D, λ[(α, β) => Δ], D0, D](newValue, δ)

  def apply[D, Δ, D0](res: Option[(D, Δ)]): UpdateResult[D, λ[(α, β) => Δ], D0] = res match {
    case Some((d, δ)) => UpdateResult(d, δ)
    case None         => UpdateResult()
  }
}

trait Dom[D] extends IDom[D] {
  type Update
  type Delta

  type IDelta[D1, D2] = Delta

  /** Associative, idempotent and monotonic (non-decreasing) operation
    * to combine diffs.
    */
  def appendDeltas(d1: Delta, d2: Delta): Delta

  final override def composeDeltas[D1, D2, D3](δ1: IDelta[D2, D3], δ2: IDelta[D1, D2]): IDelta[D1, D3] =
    appendDeltas(δ2, δ1)

  /** A variation on [[update]] that always returns the updated value,
    * whether changed or unchaged. */
  def update_(d: D, u: Update): D = update(d, u) match {
    case Updated(d1, _) => d1
    case Unchanged()    => d
  }

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

      def update[A0 <: A](a: A0, u: Update): UpdateResult[A, IDelta, A0] =
        domB.update(iso.to(a), u) match {
          case Updated(b, δ) => UpdateResult(iso.from(b), δ)
          case Unchanged()   => UpdateResult()
        }

      def appendDeltas(d1: Delta, d2: Delta): Delta =
        domB.appendDeltas(d1, d2)

      def isFailed(a: A): Boolean =
        domB.isFailed(iso.to(a))
    }

  def tuple2[D1, D2](implicit dom1: Dom[D1], dom2: Dom[D2]): Dom.Aux[(D1, D2), dom1.Update \&/ dom2.Update, dom1.Delta \&/ dom2.Delta] =
    new Dom[(D1, D2)] {
      type Update = dom1.Update \&/ dom2.Update
      type Delta = dom1.Delta \&/ dom2.Delta

      def update[D12 <: (D1, D2)](d: D12, u: Update): UpdateResult[(D1, D2), IDelta, D12] = u match {
        case \&/.This(u1) => dom1.update(d._1, u1) match {
          case Updated(d1, δ1) => UpdateResult((d1, d._2), \&/.This(δ1))
          case Unchanged()     => UpdateResult()
        }
        case \&/.That(u2) => dom2.update(d._2, u2) match {
          case Updated(d2, δ2) => UpdateResult((d._1, d2), \&/.That(δ2))
          case Unchanged()     => UpdateResult()
        }
        case \&/.Both(u1, u2) => (dom1.update(d._1, u1), dom2.update(d._2, u2)) match {
          case (Updated(d1, δ1), Updated(d2, δ2)) => UpdateResult((d1, d2  ), \&/.Both(δ1, δ2))
          case (Updated(d1, δ1), Unchanged()    ) => UpdateResult((d1, d._2), \&/.This(δ1))
          case (Unchanged()    , Updated(d2, δ2)) => UpdateResult((d._1, d2), \&/.That(δ2))
          case (Unchanged()    , Unchanged()    ) => UpdateResult()
        }
      }

      def appendDeltas(δ1: Delta, δ2: Delta): Delta =
        deltaSemigroup.append(δ1, δ2)

      def isFailed(d: (D1, D2)): Boolean =
        dom1.isFailed(d._1) || dom2.isFailed(d._2)

      override val deltaSemigroup = \&/.TheseSemigroup(dom1.deltaSemigroup, dom2.deltaSemigroup)
    }
}