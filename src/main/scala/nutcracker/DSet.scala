package nutcracker

import scala.language.higherKinds
import nutcracker.Dom.Status
import nutcracker.util.{ContU, Lst}

import scalaz.{Bind, Functor}
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
@deprecated("Use IncRefSet instead", since = "0.2")
case class DSet[Ref[_], D] private(unrefined: Set[Ref[D]], refined: Set[Ref[D]])

object DSet {

  sealed trait Update[Ref[_], D] {
    def ref: Ref[D]
  }
  case class Unrefined[Ref[_], D](ref: Ref[D]) extends Update[Ref, D]
  case class Refined  [Ref[_], D](ref: Ref[D]) extends Update[Ref, D]
  case class Failed   [Ref[_], D](ref: Ref[D]) extends Update[Ref, D]

  case class Inserted[Ref[_], D](refs: Lst[Ref[D]]) extends AnyVal {
    def +(that: Inserted[Ref, D]): Inserted[Ref, D] = Inserted(this.refs ++ that.refs)
  }

  object Inserted {
    def apply[Ref[_], D](ref: Ref[D]): Inserted[Ref, D] = Inserted(Lst.singleton(ref))
  }

  def empty[Ref[_], D]: DSet[Ref, D] = DSet(Set.empty, Set.empty)

  def init[F[_], Ref[_], D](implicit P: Propagation[F, Ref]): F[Ref[DSet[Ref, D]]] =
    P.newCell(empty[Ref, D])

  def includeC[F[_], Ref[_], D](cps: ContU[F, _ <: Ref[D]], ref: Ref[DSet[Ref, D]])(implicit dom: Dom[D], P: Propagation[F, Ref], F: Functor[F]): F[Unit] =
    cps(dref => insert(dref, ref))

  def collect[F[_], Ref[_], D](cps: ContU[F, _ <: Ref[D]])(implicit dom: Dom[D], P: Propagation[F, Ref], B: Bind[F]): F[Ref[DSet[Ref, D]]] =
    for {
      res <- init[F, Ref, D]
        _ <- includeC(cps, res)
    } yield res

  def insert[F[_], Ref[_], D](ref: Ref[D], into: Ref[DSet[Ref, D]])(implicit dom: Dom[D], P: Propagation[F, Ref], F: Functor[F]): F[Unit] = {
    P.observe(ref).untilRight(d => dom.assess(d) match {
      case Dom.Failed => Right(P.update(into).by(Failed(ref)))
      case Dom.Unrefined(_) => Left(P.update(into).by(Unrefined(ref)))
      case Dom.Refined => Left(P.update(into).by(Refined(ref)))
    })
  }

  implicit def domInstance[Ref[_], D]: Dom.Aux[DSet[Ref, D], Update[Ref, D], Inserted[Ref, D]] =
    new Dom[DSet[Ref, D]] {
      type Update = DSet.Update[Ref, D]
      type Delta = DSet.Inserted[Ref, D]

      def update[S <: DSet[Ref, D]](d: S, u: Update): UpdateResult[DSet[Ref, D], IDelta, S] = {
        val (unrefined, refined) = u match {
          case Unrefined(ref) => (d.unrefined + ref, d.refined - ref)
          case Refined(ref)   => (d.unrefined - ref, d.refined + ref)
          case Failed(ref)    => (d.unrefined - ref, d.refined - ref)
        }

        if(unrefined.size + refined.size > d.unrefined.size + d.refined.size)
          UpdateResult(DSet(unrefined, refined), Inserted(u.ref))
        else
          UpdateResult()
      }

      def appendDeltas(d1: Delta, d2: Delta): Delta = d1 + d2

      /** DSet is considered refined if at least one of the contained
        * domains is refined.
        */
      def assess(d: DSet[Ref, D]): Status[Update] =
        if(d.refined.nonEmpty) Dom.Refined
        else Dom.Unrefined(() => None)
    }
}
