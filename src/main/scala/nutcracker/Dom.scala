package nutcracker

import scalaz.{Semigroup, \&/}

trait Dom[D] {
  type Update
  type Delta

  /** Applies the monotonic update `u` to `d`, obtaining `d1 ≥ d`
    * and a description of the diff. If the update doesn't have any
    * effect on `d`, returns `None`. In addition to being monotonic,
    * updates also have to be _commutative_ and _idempotent_ (when
    * the deltas are combined using [[combineDeltas()]]).
    */
  def update(d: D, u: Update): Option[(D, Delta)]

  /** Associative, idempotent and monotonic (non-decreasing) operation
    * to combine diffs.
    */
  def combineDeltas(d1: Delta, d2: Delta): Delta

  def assess(d: D): Dom.Status[Update]

  /** A variation on [[update]] that always returns the updated value,
    * whether changed or unchaged. */
  def update_(d: D, u: Update): D = update(d, u) match {
    case Some((d1, _)) => d1
    case None => d
  }

  def deltaSemigroup: Semigroup[Delta] = new Semigroup[Delta] {
    def append(d1: Delta, d2: => Delta): Delta = combineDeltas(d1, d2)
  }
}

object Dom {
  type Aux[D, U, Δ] = Dom[D] { type Update = U; type Delta = Δ }

  def apply[D](implicit dom: Dom[D]): Dom.Aux[D, dom.Update, dom.Delta] = dom

  sealed trait Status[+U]
  case class Unrefined[U](xor: () => Option[List[U]]) extends Status[U]
  case object Refined extends Status[Nothing]
  case object Failed extends Status[Nothing]

  def tuple2[D1, D2](implicit dom1: Dom[D1], dom2: Dom[D2]): Dom.Aux[(D1, D2), dom1.Update \&/ dom2.Update, dom1.Delta \&/ dom2.Delta] =
    new Dom[(D1, D2)] {
      type Update = dom1.Update \&/ dom2.Update
      type Delta = dom1.Delta \&/ dom2.Delta

      def update(d: (D1, D2), u: Update): Option[((D1, D2), Delta)] = u match {
        case \&/.This(u1) => dom1.update(d._1, u1) match {
          case Some((d1, δ1)) => Some(((d1, d._2), \&/.This(δ1)))
          case None => None
        }
        case \&/.That(u2) => dom2.update(d._2, u2) match {
          case Some((d2, δ2)) => Some(((d._1, d2), \&/.That(δ2)))
          case None => None
        }
        case \&/.Both(u1, u2) => (dom1.update(d._1, u1), dom2.update(d._2, u2)) match {
          case (Some((d1, δ1)), Some((d2, δ2))) => Some(((d1, d2),   \&/.Both(δ1, δ2)))
          case (Some((d1, δ1)), None          ) => Some(((d1, d._2), \&/.This(δ1)))
          case (None,           Some((d2, δ2))) => Some(((d._1, d2), \&/.That(δ2)))
          case (None,           None          ) => None
        }
      }

      def combineDeltas(δ1: Delta, δ2: Delta): Delta = deltaSemigroup.append(δ1, δ2)

      def assess(d: (D1, D2)): Status[Update] = (dom1.assess(d._1), dom2.assess(d._2)) match {
        case (Failed, _) => Failed
        case (_, Failed) => Failed
        case (Refined, Refined) => Refined
        case (Unrefined(u1), Refined) => Unrefined(() => u1().map(_.map(\&/.This(_))))
        case (Refined, Unrefined(u2)) => Unrefined(() => u2().map(_.map(\&/.That(_))))
        case (Unrefined(u1), Unrefined(u2)) => Unrefined(() => {
          u1() match {
            case None => u2().map(_.map(\&/.That(_)))
            case Some(u1s) => u2() match {
              case None => Some(u1s.map(\&/.This(_)))
              case Some(u2s) => Some(for { u1 <- u1s; u2 <- u2s } yield \&/.Both(u1, u2)) // Cartesian product
            }
          }
        })
      }

      override val deltaSemigroup = \&/.TheseSemigroup(dom1.deltaSemigroup, dom2.deltaSemigroup)
    }
}