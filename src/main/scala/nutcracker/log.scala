package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions

import nutcracker.Dom.Status

/** Convenience methods to work with a list as a prepend-only log.
  * Monotonic update is prepending an element to the list.
  * A diff is a list of appended elements.
  *
  * **Note:** updates to log are neither idempotent nor commutative,
  * as is required by [[Dom.update()]], so don't base any business
  * logic on it.
  */
object log {

  type LogRef[A] = DRef[List[A]]

  final case class LogOps[A](l: LogRef[A]) extends AnyVal {
    def write[F[_]: Propagation](a: A): F[Unit] = log(l, a)
  }

  implicit def logOps[A](l: LogRef[A]): LogOps[A] = LogOps(l)

  def newLog[F[_], A](implicit P: Propagation[F]): F[LogRef[A]] = P.cell(List[A]())
  def log[F[_], A](ref: LogRef[A], a: A)(implicit P: Propagation[F]): F[Unit] = P.update(ref).by(a)

  implicit def logDom[A]: Dom.Aux[List[A], A, List[A]] = new Dom[List[A]] {
    type Update = A
    type Delta = List[A]

    override def update(d: List[A], u: A): Option[(List[A], List[A])] =
      Some((u :: d, List(u)))

    override def combineDeltas(d1: List[A], d2: List[A]): List[A] = d2 ::: d1

    override def assess(d: List[A]): Status[A] = Dom.Refined
  }
}
