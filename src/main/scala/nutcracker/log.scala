package nutcracker

import scala.language.implicitConversions

import nutcracker.Dom.Status
import nutcracker.PropagationLang._
import nutcracker.util.FreeK

/** Convenience methods to work with a list as a prepend-only log.
  * Monotonic update is prepending an element to the list.
  * A diff is a list of appended elements.
  *
  * **Note:** updates to log are neither idempotent nor commutative,
  * as is required by [[Dom.update()]], so don't base any business
  * logic on it.
  */
object log {

  type LogRef[A] = DRef[List[A], A, List[A]]

  final case class LogOps[A](l: LogRef[A]) extends AnyVal {
    def write(a: A): FreeK[PropagationLang, Unit] = log(l, a)
  }

  implicit def logOps[A](l: LogRef[A]): LogOps[A] = LogOps(l)

  def newLog[A]: FreeK[PropagationLang, LogRef[A]] = cellF(List[A]())
  def log[A](ref: LogRef[A], a: A): FreeK[PropagationLang, Unit] = updateF(ref)(a)

  implicit def logDom[A]: Dom[List[A], A, List[A]] = new Dom[List[A], A, List[A]] {

    def update(d: List[A], u: A): Option[(List[A], List[A])] =
      Some((u :: d, List(u)))

    def combineDiffs(d1: List[A], d2: List[A]): List[A] = d2 ::: d1

    def assess(d: List[A]): Status[A] = Dom.Refined
  }
}
