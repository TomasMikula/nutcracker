package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions

/** Convenience methods to work with a list as a prepend-only log.
  * Monotonic update is prepending an element to the list.
  * A diff is a list of appended elements.
  *
  * **Note:** updates to log are neither idempotent nor commutative,
  * as is required by [[Dom.update]], so don't base any business
  * logic on it.
  */
object log {

  type Log[A] = List[A]

  final case class LogOps[F[_], Var[_], Val[_], A](l: Var[Log[A]])(implicit P: Propagation[F, Var, Val]) {
    def write(a: A): F[Unit] = log(l, a)
  }

  implicit def logOps[F[_], Var[_], Val[_], A](l: Var[Log[A]])(implicit P: Propagation[F, Var, Val]): LogOps[F, Var, Val, A] = LogOps(l)

  def newLog[A]: NewLogSyntaxHelper[A] = NewLogSyntaxHelper[A]()
  def log[F[_], Var[_], Val[_], A](ref: Var[Log[A]], a: A)(implicit P: Propagation[F, Var, Val]): F[Unit] = P.update(ref).by(a)

  final case class NewLogSyntaxHelper[A]() {
    def apply[F[_], Var[_], Val[_]]()(implicit P: Propagation[F, Var, Val]): F[Var[Log[A]]] = P.newCell(List[A]())
  }

  implicit def logDom[A]: Dom.Aux[Log[A], A, Log[A]] = new Dom[Log[A]] {
    type Update = A
    type Delta = Log[A]

    override def update[L <: Log[A]](d: L, u: A): UpdateResult[Log[A], IDelta, L] =
      UpdateResult(u :: d, List(u))

    override def appendDeltas(d1: Log[A], d2: Log[A]): Log[A] = d2 ::: d1

    override def isFailed(d: Log[A]) = false
  }
}
