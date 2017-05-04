package nutcracker.data

import nutcracker.{Dom, DomWithBottom, UpdateResult}
import scalaz.\&/.{Both, That, This}
import scalaz.{-\/, \&/, \/, \/-}

final case class Lexicographic[A, B](_1: A, _2: B)

object Lexicographic {
  implicit def domInstance[A, B](implicit A: Dom[A], B: DomWithBottom[B]): Dom.Aux[Lexicographic[A, B], A.Update \/ B.Update, A.Delta \&/ B.Delta] =
    new Dom[Lexicographic[A, B]] {
      type Update = A.Update \/ B.Update
      type Delta = A.Delta \&/ B.Delta

      override def update[D0 <: Lexicographic[A, B]](d: D0, u: Update): UpdateResult[Lexicographic[A, B], IDelta, D0] =
        u match {
          case -\/(ua) => A.update(d._1, ua).map(Lexicographic(_, B.bottom), This(_))
          case \/-(ub) => B.update(d._2, ub).map(Lexicographic(d._1, _),     That(_))
        }

      override def appendDeltas(d1: Delta, d2: Delta): Delta = d2 match {
        case This(da2) => This(d1.a match {
          case Some(da1) => A.appendDeltas(da1, da2)
          case None => da2
        })
        case Both(da2, db2) => Both(d1.a match {
          case Some(da1) => A.appendDeltas(da1, da2)
          case None => da2
        }, db2)
        case That(db2) => d1 match {
          case This(da1) => Both(da1, db2)
          case Both(da1, db1) => Both(da1, B.appendDeltas(db1, db2))
          case That(db1) => That(B.appendDeltas(db1, db2))
        }
      }

      override def isFailed(d: Lexicographic[A, B]): Boolean =
        A.isFailed(d._1)
    }
}