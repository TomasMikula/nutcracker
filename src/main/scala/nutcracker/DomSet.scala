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
final case class DomSet[Ref[_], A](value: Set[Ref[A]]) extends AnyVal

object DomSet {

  sealed trait Update[Ref[_], A]
  final case class Insert[Ref[_], A](ref: Ref[A]) extends Update[Ref, A]
  final case class Failed[Ref[_], A](ref: Ref[A]) extends Update[Ref, A]

  final case class Inserted[Ref[_], A](refs: Lst[Ref[A]]) extends AnyVal

  type Delta[Ref[_], A] = Inserted[Ref, A]


  def empty[Ref[_], A]: DomSet[Ref, A] = DomSet(Set.empty)

  def init[F[_], Ref[_], A](implicit P: Propagation[F, Ref]): F[Ref[DomSet[Ref, A]]] =
    P.newCell(empty[Ref, A])

  def includeC[F[_], Ref[_], A](cps: ContU[F, _ <: Ref[A]], ref: Ref[DomSet[Ref, A]])(implicit P: Propagation[F, Ref], dom: Dom[A]): F[Unit] =
    cps(dref => insert(dref, ref))

  def collect[F[_]: Bind, Ref[_], A](cps: ContU[F, _ <: Ref[A]])(implicit P: Propagation[F, Ref], dom: Dom[A]): F[Ref[DomSet[Ref, A]]] =
    for {
      res <- init[F, Ref, A]
      _ <- includeC(cps, res)
    } yield res

  def insert[F[_], Ref[_], A](ref: Ref[A], into: Ref[DomSet[Ref, A]])(implicit P: Propagation[F, Ref], dom: Dom[A]): F[Unit] =
    P.valTrigger(ref)(d => dom.assess(d) match {
      case Dom.Failed => Fire(P.update(into).by(Failed(ref)))
      case _ => FireReload(P.update(into).by(Insert(ref)))
    })

  implicit def domInstance[Ref[_], A]: Dom.Aux[DomSet[Ref, A], Update[Ref, A], Delta[Ref, A]] = new Dom[DomSet[Ref, A]] {
    type Update = DomSet.Update[Ref, A]
    type Delta  = DomSet.Delta[Ref, A]

    def update(d: DomSet[Ref, A], u: Update): Option[(DomSet[Ref, A], Delta)] = u match {
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

    def assess(d: DomSet[Ref, A]): Status[Update] = Refined
  }
}