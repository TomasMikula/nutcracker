package nutcracker

import scala.language.higherKinds

import nutcracker.Dom.{Failed, Refined, Status}

sealed trait Revocable[+A]

final case class Valid[A](value: A) extends Revocable[A]
final case object Revoked extends Revocable[Nothing]

object Revocable {
  /** Meaning of update is to revoke the value. */
  type Update = Unit

  /** When notification of type Delta arrives, it means the value has been revoked. */
  type Delta = Unit

  def apply[A](a: A): Revocable[A] = Valid(a)

  def init[F[_], Ref[_], A](a: A)(implicit P: Propagation[F, Ref]): F[Ref[Revocable[A]]] =
    P.newCell(Revocable(a))

  def revoke[F[_], Ref[_], A](ref: Ref[Revocable[A]])(implicit P: Propagation[F, Ref]): F[Unit] =
    P.update(ref).by(())

  implicit def domInstance[A]: Dom.Aux[Revocable[A], Update, Delta] = new Dom[Revocable[A]] {
    type Update = Revocable.Update
    type Delta = Revocable.Delta

    def update[R <: Revocable[A]](d: R, u: Update): UpdateResult[Revocable[A], IDelta, R] = d match {
      case Valid(a) => UpdateResult(Revoked, ())
      case Revoked  => UpdateResult()
    }

    def appendDeltas(d1: Delta, d2: Delta): Delta = ()

    def assess(d: Revocable[A]): Status[Update] = d match {
      case Valid(a) => Refined
      case Revoked  => Failed
    }
  }
}