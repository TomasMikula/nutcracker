package nutcracker

import scala.language.higherKinds

import nutcracker.Dom.{Refined, Status}
import nutcracker.DomSet.{Delta, Update}
import nutcracker.PropagationLang.{cellF, updateF, valTriggerF}
import nutcracker.util.{ContF, FreeK, InjectK, Lst}

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

  type Ref[A] = DRef.Aux[DomSet[A], Update[A], Delta[A]]


  def empty[A]: DomSet[A] = DomSet(Set.empty)

  def init[F[_[_], _], A](implicit inj: InjectK[PropagationLang, F]): FreeK[F, Ref[A]] =
    cellF(empty[A]).inject[F]

  def includeC[F[_[_], _], A](cps: ContF[F, _ <: DRef[A]], ref: Ref[A])(implicit inj: InjectK[PropagationLang, F], dom: Dom[A]): FreeK[F, Unit] =
    cps(dref => insert(dref, ref))

  def collect[F[_[_], _], A](cps: ContF[F, _ <: DRef[A]])(implicit inj: InjectK[PropagationLang, F], dom: Dom[A]): FreeK[F, Ref[A]] =
    for {
      res <- init[F, A]
      _ <- includeC(cps, res)
    } yield res

  def insert[F[_[_], _], A](ref: DRef[A], into: Ref[A])(implicit inj: InjectK[PropagationLang, F], dom: Dom[A]): FreeK[F, Unit] =
    valTriggerF(ref)(d => dom.assess(d) match {
      case Dom.Failed => Fire(updateF(into)(Failed(ref)))
      case _ => FireReload(updateF(into)(Insert(ref)))
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