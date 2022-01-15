package nutcracker.data

import nutcracker.{Dom, Propagation, UpdateResult}
import scala.language.implicitConversions

/** Convenience methods to work with a list as a prepend-only log.
  * Monotonic update is prepending an element to the list.
  * A diff is a list of appended elements.
  *
  * **Note:** updates to log are neither idempotent nor commutative,
  * as is required by [[Dom.update]], so don't base any business
  * logic on it.
  */
object listLog {

  type Log[A] = List[A]

  final case class LogOps[F[_], Var[_], A](l: Var[Log[A]])(implicit P: Propagation.Aux0[F, Var]) {
    def write(a: A): F[Unit] = log(l, a)
  }

  implicit def logOps[F[_], Var[_], A](l: Var[Log[A]])(implicit P: Propagation.Aux0[F, Var]): LogOps[F, Var, A] = LogOps(l)

  def newLog[A]: NewLogSyntaxHelper[A] = NewLogSyntaxHelper[A]()
  def log[F[_], Var[_], A](ref: Var[Log[A]], a: A)(implicit P: Propagation.Aux0[F, Var]): F[Unit] = P.update(ref).by(a)

  final case class NewLogSyntaxHelper[A]() {
    def apply[F[_], Var[_]]()(implicit P: Propagation.Aux0[F, Var]): F[Var[Log[A]]] = P.newCell(List[A]())
  }

  implicit def logDom[A]: Dom.Aux[Log[A], A, Log[A]] = new Dom[Log[A]] {
    type Update = A
    type Delta = Log[A]

    override def update(d: Log[A], u: A): UpdateResult[Log[A], Delta] =
      UpdateResult.updated(u :: d, List(u))

    override def appendDeltas(d1: Log[A], d2: Log[A]): Log[A] = d2 ::: d1

    override def isFailed(d: Log[A]) = false
  }
}
