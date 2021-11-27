package nutcracker.data

import nutcracker.{Dom, Propagation, TerminalDom, UpdateResult}

/** Represents a discrete (non-refinable) value that can be revoked.
  * Is isomorphic to `Closeable[Discrete[A]]`.
  *
  * @see [[Discrete]], [[Closeable]]
  */
sealed trait Revocable[+A]

object Revocable {
  case class Valid[A](value: A) extends Revocable[A]
  case object Revoked extends Revocable[Nothing]

  /** Meaning of update is to revoke the value. */
  type Update = Unit

  /** When notification of type Delta arrives, it means the value has been revoked. */
  type Delta = Unit

  def apply[A](a: A): Revocable[A] = Valid(a)

  def init[F[_], Var[_], Val[_], A](a: A)(implicit P: Propagation[F, Var, Val]): F[Var[Revocable[A]]] =
    P.newCell(Revocable(a))

  def revoke[F[_], Var[_], Val[_], A](ref: Var[Revocable[A]])(implicit P: Propagation[F, Var, Val]): F[Unit] =
    P.update(ref).by(())

  implicit def domInstance[A]: Dom.Aux[Revocable[A], Update, Delta] with TerminalDom[Revocable[A]] =
    new TerminalDom[Revocable[A]] {
      type Update = Revocable.Update
      type Delta = Revocable.Delta

      def update[R <: Revocable[A]](d: R, u: Update): UpdateResult[Revocable[A], IDelta, R] = d match {
        case Valid(a) => UpdateResult(Revoked, ())
        case Revoked  => UpdateResult()
      }

      def appendDeltas(d1: Delta, d2: Delta): Delta =
        sys.error("There can never be two deltas, since after revocation, there can be no other change")

      def isFailed(d: Revocable[A]): Boolean = d match {
        case Valid(a) => false
        case Revoked  => true
      }

      def terminate: Update = ()
    }
}