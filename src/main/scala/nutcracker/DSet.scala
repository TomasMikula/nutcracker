package nutcracker

import nutcracker.Dom.Status
import nutcracker.PropagationLang._
import nutcracker.util.FreeK

/** A set of domain references.
  *
  * Monotonic updates are
  *  - adding a domain reference;
  *  - a monotonic update on a member domain.
  *
  * It is considered refined when at least one of the member domains is refined.
  *
  * Note that the status does not necessarily transition only along the path
  * Unrefined -> Refined -> Failed, since a refined state can go to unrefined
  * when all the refined members become failed.
  */
case class DSet[D] private(unrefined: Set[DRef[D]], refined: Set[DRef[D]])

object DSet {

  sealed trait Update[D] {
    def ref: DRef[D];
    def toDelta: Delta[D]
  }
  case class Unrefined[D](ref: DRef[D]) extends Update[D] { def toDelta = Delta.unrefined(ref) }
  case class Refined  [D](ref: DRef[D]) extends Update[D] { def toDelta = Delta.refined(ref) }
  case class Failed   [D](ref: DRef[D]) extends Update[D] { def toDelta = Delta.failed(ref) }

  case class Delta[D](
    unrefined: Set[DRef[D]],
    refined:   Set[DRef[D]],
    failed:    Set[DRef[D]]
  ) {
    def +(that: Delta[D]): Delta[D] = Delta(
      this.unrefined union that.unrefined,
      this.refined   union that.refined,
      this.failed    union that.failed
    )
  }

  object Delta {
    def unrefined[D](ref: DRef[D]): Delta[D] = Delta(Set(ref), Set(), Set())
    def   refined[D](ref: DRef[D]): Delta[D] = Delta(Set(), Set(ref), Set())
    def    failed[D](ref: DRef[D]): Delta[D] = Delta(Set(), Set(), Set(ref))
  }

  type DSetRef[D] = DRef.Aux[DSet[D], Update[D], Delta[D]]

  def insert[D](dsref: DSetRef[D], ref: DRef[D])(implicit dom: Dom[D]): FreeK[PropagationLang, Unit] =
    valTriggerF(ref)(d => dom.assess(d) match {
      case Dom.Failed => Fire(updateF(dsref)(Failed(ref)))
      case Dom.Unrefined(_) => FireReload(updateF(dsref)(Unrefined(ref)))
      case Dom.Refined => FireReload(updateF(dsref)(Refined(ref)))
    })

  implicit def domInstance[D]: Dom.Aux[DSet[D], Update[D], Delta[D]] =
    new Dom[DSet[D]] {
      type Update = DSet.Update[D]
      type Delta = DSet.Delta[D]

      def update(d: DSet[D], u: Update): Option[(DSet[D], Delta)] = {
        val (unrefined, refined) = u match {
          case Unrefined(ref) => (d.unrefined + ref, d.refined - ref)
          case Refined(ref)   => (d.unrefined - ref, d.refined + ref)
          case Failed(ref)    => (d.unrefined - ref, d.refined - ref)
        }

        if(unrefined.size == d.unrefined.size && refined.size == d.refined.size)
          None
        else
          Some((DSet(unrefined, refined), u.toDelta))
      }

      def combineDeltas(d1: Delta, d2: Delta): Delta = d1 + d2

      /** DSet is considered refined if at least one of the contained
        * domains is refined.
        */
      def assess(d: DSet[D]): Status[Update] =
        if(d.refined.nonEmpty) Dom.Refined
        else Dom.Unrefined(() => None)
    }
}