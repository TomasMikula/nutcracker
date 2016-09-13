package nutcracker

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

  implicit def domInstance[A]: Dom.Aux[Revocable[A], Update, Delta] = new Dom[Revocable[A]] {
    type Update = Revocable.Update
    type Delta = Revocable.Delta

    def update(d: Revocable[A], u: Update): Option[(Revocable[A], Delta)] = d match {
      case Valid(a) => Some((Revoked, ()))
      case Revoked  => None
    }

    def combineDeltas(d1: Delta, d2: Delta): Delta = ()

    def assess(d: Revocable[A]): Status[Update] = d match {
      case Valid(a) => Refined
      case Revoked  => Failed
    }
  }
}