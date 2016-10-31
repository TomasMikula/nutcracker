package nutcracker

import scala.language.higherKinds
import nutcracker.Dom.{Refined, Status}
import nutcracker.util.{ContU, Lst}

import scalaz.Bind
import scalaz.syntax.bind._

/** A set of domain references, with auto-cleaning failed domains.
  *
  * Monotonic updates are adding more references to the set.
  *
  * It is considered refined at all times.
  */
final case class DomSet[A](value: Set[DRef[A]]) extends AnyVal

object DomSet {

  sealed trait Update[A]
  final case class Insert[A](ref: DRef[A]) extends Update[A]
  final case class Failed[A](ref: DRef[A]) extends Update[A]

  final case class Inserted[A](refs: Lst[DRef[A]]) extends AnyVal

  type Delta[A] = Inserted[A]

  type Ref[A] = DRef[DomSet[A]]


  def empty[A]: DomSet[A] = DomSet(Set.empty)

  def init[F[_], A](implicit P: Propagation[F]): F[Ref[A]] =
    P.cell(empty[A])

  def includeC[F[_], A](cps: ContU[F, _ <: DRef[A]], ref: Ref[A])(implicit P: Propagation[F], dom: Dom[A]): F[Unit] =
    cps(dref => insert(dref, ref))

  def collect[F[_]: Propagation: Bind, A](cps: ContU[F, _ <: DRef[A]])(implicit dom: Dom[A]): F[Ref[A]] =
    for {
      res <- init[F, A]
      _ <- includeC(cps, res)
    } yield res

  def insert[F[_], A](ref: DRef[A], into: Ref[A])(implicit P: Propagation[F], dom: Dom[A]): F[Unit] =
    P.valTrigger(ref)(d => dom.assess(d) match {
      case Dom.Failed => Fire(P.update(into).by(Failed(ref)))
      case _ => FireReload(P.update(into).by(Insert(ref)))
    })

  implicit def domInstance[A]: Dom.Aux[DomSet[A], Update[A], Delta[A]] = new Dom[DomSet[A]] {
    type Update = DomSet.Update[A]
    type Delta  = DomSet.Delta[A]

    def update(d: DomSet[A], u: Update): Option[(DomSet[A], Delta)] = u match {
      case Insert(ref) =>
        val refs = d.value + ref
        if(refs.size > d.value.size) Some((DomSet(refs), Inserted(Lst.singleton(ref))))
        else None
      case Failed(ref) =>
        val refs = d.value - ref
        if(refs.size < d.value.size) Some((DomSet(refs), Inserted(Lst.empty))) // we don't publish auto-cleaned refs
        else None
    }

    def combineDeltas(d1: Delta, d2: Delta): Delta = Inserted(d1.refs ++ d2.refs)

    def assess(d: DomSet[A]): Status[Update] = Refined
  }
}