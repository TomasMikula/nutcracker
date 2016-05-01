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
case class DSet[D, U, Δ] private(unrefined: Set[DRef[D, U, Δ]], refined: Set[DRef[D, U, Δ]])

object DSet {

  sealed trait Update[D, U, Δ] {
    def ref: DRef[D, U, Δ];
    def toDelta: Delta[D, U, Δ]
  }
  case class Unrefined[D, U, Δ](ref: DRef[D, U, Δ]) extends Update[D, U, Δ] { def toDelta = Delta.unrefined(ref) }
  case class Refined  [D, U, Δ](ref: DRef[D, U, Δ]) extends Update[D, U, Δ] { def toDelta = Delta.refined(ref) }
  case class Failed   [D, U, Δ](ref: DRef[D, U, Δ]) extends Update[D, U, Δ] { def toDelta = Delta.failed(ref) }

  case class Delta[D, U, Δ](
    unrefined: Set[DRef[D, U, Δ]],
    refined:   Set[DRef[D, U, Δ]],
    failed:    Set[DRef[D, U, Δ]]
  ) {
    def +(that: Delta[D, U, Δ]): Delta[D, U, Δ] = Delta(
      this.unrefined union that.unrefined,
      this.refined   union that.refined,
      this.failed    union that.failed
    )
  }

  object Delta {
    def unrefined[D, U, Δ](ref: DRef[D, U, Δ]): Delta[D, U, Δ] = Delta(Set(ref), Set(), Set())
    def   refined[D, U, Δ](ref: DRef[D, U, Δ]): Delta[D, U, Δ] = Delta(Set(), Set(ref), Set())
    def    failed[D, U, Δ](ref: DRef[D, U, Δ]): Delta[D, U, Δ] = Delta(Set(), Set(), Set(ref))
  }

  type DSetRef[D, U, Δ] = DRef[DSet[D, U, Δ], Update[D, U, Δ], Delta[D, U, Δ]]

  def insert[D, U, Δ](dsref: DSetRef[D, U, Δ], ref: DRef[D, U, Δ])(implicit dom: Dom[D, U, Δ]): FreeK[PropagationLang, Unit] =
    valTriggerF(ref)(d => dom.assess(d) match {
      case Dom.Failed => Fire(updateF(dsref)(Failed(ref)))
      case Dom.Unrefined(_) => FireReload(updateF(dsref)(Unrefined(ref)))
      case Dom.Refined => FireReload(updateF(dsref)(Refined(ref)))
    })

  implicit def domInstance[D, U, Δ]: Dom[DSet[D, U, Δ], Update[D, U, Δ], Delta[D, U, Δ]] =
    new Dom[DSet[D, U, Δ], Update[D, U, Δ], Delta[D, U, Δ]] {

      def update(d: DSet[D, U, Δ], u: Update[D, U, Δ]): Option[(DSet[D, U, Δ], Delta[D, U, Δ])] = {
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

      def combineDiffs(d1: Delta[D, U, Δ], d2: Delta[D, U, Δ]): Delta[D, U, Δ] = d1 + d2

      /** DSet is considered refined if at least one of the contained
        * domains is refined.
        */
      def assess(d: DSet[D, U, Δ]): Status[Update[D, U, Δ]] =
        if(d.refined.nonEmpty) Dom.Refined
        else Dom.Unrefined(() => None)
    }
}