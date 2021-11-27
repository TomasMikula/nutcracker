package nutcracker.data

import nutcracker.{Dom, TerminalDom, UpdateResult}

/**
  * Represents a refinable value that in addition can be artificially revoked ("closed").
  */
sealed trait Closeable[+A]

object Closeable {
  case class Open[A](value: A) extends Closeable[A]
  case object Closed extends Closeable[Nothing]

  sealed abstract class Update[+U]
  case class UpdateContent[U](value: U) extends Update[U]
  case object Terminate extends Update[Nothing]

  sealed abstract class Delta[+Δ]
  case class ContentUpdated[Δ](value: Δ) extends Delta[Δ]
  case object Terminated extends Delta[Nothing]

  implicit def domInstance[A](implicit A: Dom[A]): Dom.Aux[Closeable[A], Update[A.Update], Delta[A.Delta]] with TerminalDom[Closeable[A]] =
    new TerminalDom[Closeable[A]] {
      type Update = Closeable.Update[A.Update]
      type Delta = Closeable.Delta[A.Delta]

      override def update[D0 <: Closeable[A]](d: D0, u: Update): UpdateResult[Closeable[A], IDelta, D0] = d match {
        case Open(a) => u match {
          case UpdateContent(ua) => A.update(a, ua).map(Open(_), ContentUpdated(_))
          case Terminate => UpdateResult(Closed, Terminated)
        }
        case Closed => UpdateResult()
      }

      override def appendDeltas(d1: Delta, d2: Delta): Delta =
        d1 match {
          case ContentUpdated(δ1) => d2 match {
            case ContentUpdated(δ2) => ContentUpdated(A.appendDeltas(δ1, δ2))
            case Terminated => Terminated
          }
          case Terminated =>
            sys.error("After termination (δ1), there can be no other delta (δ2)")
        }

      override def isFailed(d: Closeable[A]): Boolean = d match {
        case Open(a) => A.isFailed(a)
        case Closed => true
      }

      override def terminate: Update = Terminate
    }
}