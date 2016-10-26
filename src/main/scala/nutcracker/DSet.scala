package nutcracker

import scala.language.higherKinds
import nutcracker.Dom.Status
import nutcracker.PropagationLang._
import nutcracker.util.{ContU, Lst}

import scalaz.Bind
import scalaz.syntax.monad._

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
    def ref: DRef[D]
  }
  case class Unrefined[D](ref: DRef[D]) extends Update[D]
  case class Refined  [D](ref: DRef[D]) extends Update[D]
  case class Failed   [D](ref: DRef[D]) extends Update[D]

  case class Inserted[D](refs: Lst[DRef[D]]) extends AnyVal {
    def +(that: Inserted[D]): Inserted[D] = Inserted(this.refs ++ that.refs)
  }

  object Inserted {
    def apply[D](ref: DRef[D]): Inserted[D] = Inserted(Lst.singleton(ref))
  }

  type DSetRef[D] = DRef.Aux[DSet[D], Update[D], Inserted[D]]

  def empty[D]: DSet[D] = DSet(Set.empty, Set.empty)

  def init[F[_], D](implicit P: Propagation[F]): F[DSetRef[D]] =
    P.cell(empty[D])

  def includeC[F[_], D](cps: ContU[F, _ <: DRef[D]], ref: DSetRef[D])(implicit dom: Dom[D], P: Propagation[F]): F[Unit] =
    cps(dref => insert(dref, ref))

  def collect[F[_], D](cps: ContU[F, _ <: DRef[D]])(implicit dom: Dom[D], P: Propagation[F], B: Bind[F]): F[DSetRef[D]] =
    for {
      res <- init[F, D]
        _ <- includeC(cps, res)
    } yield res

  def insert[F[_], D](ref: DRef[D], into: DSetRef[D])(implicit dom: Dom[D], P: Propagation[F]): F[Unit] =
    P.valTrigger(ref)(d => dom.assess(d) match {
      case Dom.Failed => Fire(P.update(into)(Failed(ref)))
      case Dom.Unrefined(_) => FireReload(P.update(into)(Unrefined(ref)))
      case Dom.Refined => FireReload(P.update(into)(Refined(ref)))
    })

  implicit def domInstance[D]: Dom.Aux[DSet[D], Update[D], Inserted[D]] =
    new Dom[DSet[D]] {
      type Update = DSet.Update[D]
      type Delta = DSet.Inserted[D]

      def update(d: DSet[D], u: Update): Option[(DSet[D], Delta)] = {
        val (unrefined, refined) = u match {
          case Unrefined(ref) => (d.unrefined + ref, d.refined - ref)
          case Refined(ref)   => (d.unrefined - ref, d.refined + ref)
          case Failed(ref)    => (d.unrefined - ref, d.refined - ref)
        }

        if(unrefined.size + refined.size > d.unrefined.size + d.refined.size)
          Some((DSet(unrefined, refined), Inserted(u.ref)))
        else
          None
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
