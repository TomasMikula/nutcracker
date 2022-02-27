package nutcracker.data

import nutcracker.util.{DeepEqual, DeepShow, IsEqual, MonadObjectOutput}
import nutcracker.{Final, JoinDom}
import scalaz.Equal

/** Like [[Promise]], but it is a conflict to complete a [[PromiseOnce]] twice,
  * regardless of the value it is completed with.
  *
  * @see [[Promise]]
  */
sealed trait PromiseOnce[+A] {
  import Promise.{Conflict, Empty}

  def isEmpty: Boolean = this == Empty
  def nonEmpty: Boolean = this != Empty
  def isCompleted: Boolean = !(isEmpty || isConflict)
  def isConflict: Boolean = this == Conflict
}

object PromiseOnce {

  case object Empty extends PromiseOnce[Nothing] // incomplete promise
  case class Completed[A](value: A) extends PromiseOnce[A]
  case object Conflict extends PromiseOnce[Nothing] // promise completed multiple times

  type Update[A] = PromiseOnce[A]
  type Delta[A] = Unit

  def empty[A]: PromiseOnce[A] = Empty
  def completed[A](a: A): PromiseOnce[A] = Completed(a)

  implicit def finalInstance[A]: Final.Aux[PromiseOnce[A], A] =
    new Final[PromiseOnce[A]] {
      type Out = A

      def extract(pa: PromiseOnce[A]): Option[A] = pa match {
        case Completed(a) => Some(a)
        case _ => None
      }

      def embed(a: A): PromiseOnce[A] = Completed(a)
    }

  implicit def promiseOnceDomain[A]: JoinDom.Template[PromiseOnce[A]] =
    new JoinDom.Template[PromiseOnce[A]] {

      override def isFailed(pa: PromiseOnce[A]): Boolean = pa match {
        case Conflict => true
        case _ => false
      }

      override def ljoin0(d1: PromiseOnce[A], d2: PromiseOnce[A]): Option[PromiseOnce[A]] =
        (d1, d2) match {
          case (_, Empty) => None
          case (Empty, c2 @ Completed(_)) => Some(c2)
          case (Completed(a1) , Completed(a2)) => Some(Conflict)
          case (Conflict, _) => None
          case (_, Conflict) => Some(Conflict)
        }

      override def appendDeltas(d1: Unit, d2: Unit): Unit = ()
    }

  implicit def equalInstance[A](implicit A: Equal[A]): Equal[PromiseOnce[A]] =
    new Equal[PromiseOnce[A]] {
      def equal(p1: PromiseOnce[A], p2: PromiseOnce[A]): Boolean = (p1, p2) match {
        case (Completed(a1), Completed(a2)) => A.equal(a1, a2)
        case (Empty, Empty) => true
        case (Conflict, Conflict) => true
        case _ => false
      }
    }

  implicit def deepEqualInstance[A, B, Ptr1[_], Ptr2[_]](implicit ev: DeepEqual[A, B, Ptr1, Ptr2]): DeepEqual[PromiseOnce[A], PromiseOnce[B], Ptr1, Ptr2] =
    new DeepEqual[PromiseOnce[A], PromiseOnce[B], Ptr1, Ptr2] {
      def equal(p1: PromiseOnce[A], p2: PromiseOnce[B]): IsEqual[Ptr1, Ptr2] = (p1, p2) match {
        case (Completed(a1), Completed(a2)) => ev.equal(a1, a2)
        case (Empty, Empty) => IsEqual(true)
        case (Conflict, Conflict) => IsEqual(true)
        case _ => IsEqual(false)
      }
    }

  implicit def deepShowInstance[A, Ptr[_]](implicit ev: DeepShow[A, Ptr]): DeepShow[PromiseOnce[A], Ptr] =
    new DeepShow.FromSerialize[PromiseOnce[A], Ptr] {
      def serialize[M[_]](a: PromiseOnce[A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] = a match {
        case Empty => M.write("?")
        case Conflict => M.write("⊥")
        case Completed(a) => ev.serialize(a)
      }
    }
}