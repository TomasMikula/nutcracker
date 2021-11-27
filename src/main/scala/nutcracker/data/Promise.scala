package nutcracker.data

import nutcracker.util.{DeepEqual, DeepShow, IsEqual, MonadObjectOutput}
import nutcracker.{Final, JoinDom}
import scalaz.Equal
import scalaz.syntax.equal._

/** For any type `A`, `Promise[A]` is a bounded lattice on the set `A ⊔ {0, 1}`
  * (where "⊔" means "disjoint union") with `0` being the least element, `1`
  * being the greatest element, and all elements of `A` forming an antichain
  * (i.e. mutually incomparable). `0` represents an empty (incomplete) promise,
  * the elements of `A` represent a completed promise, and `1` represents a
  * conflict (a promise completed multiple times with different values).
  *
  * @see [[Revocable]]
  * @see [[Discrete]]
  */
sealed trait Promise[+A] {
  import Promise.{Conflict, Empty}

  def isEmpty: Boolean = this == Empty
  def nonEmpty: Boolean = this != Empty
  def isCompleted: Boolean = !(isEmpty || isConflict)
  def isConflict: Boolean = this == Conflict
}

object Promise {

  case object Empty extends Promise[Nothing] // incomplete promise
  case class Completed[A](value: A) extends Promise[A]
  case object Conflict extends Promise[Nothing] // promise completed multiple times with different values

  type Update[A] = Promise[A]
  type Delta[A] = Unit
  type IDelta[A, D1, D2] = Delta[A]

  def empty[A]: Promise[A] = Empty
  def completed[A](a: A): Promise[A] = Completed(a)

  def meet[A: Equal](p1: Promise[A], p2: Promise[A]): Promise[A] = (p1, p2) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Conflict, p2) => p2
    case (p1, Conflict) => p1
    case (Completed(a1), Completed(a2)) => if(a1 === a2) p1 else Empty
  }

  implicit def finalInstance[A]: Final.Aux[Promise[A], A] = new Final[Promise[A]] {
    type Out = A

    def extract(pa: Promise[A]): Option[A] = pa match {
      case Completed(a) => Some(a)
      case _ => None
    }

    def embed(a: A): Promise[A] = Completed(a)
  }

  implicit def promiseDomain[A](implicit EqA: Equal[A]): JoinDom.Template[Promise[A]] =
    new JoinDom.Template[Promise[A]] {

      override def isFailed(pa: Promise[A]): Boolean = pa match {
        case Conflict => true
        case _ => false
      }

      override def ljoin0(d1: Promise[A], d2: Promise[A]): Option[Promise[A]] = (d1, d2) match {
        case (_, Empty) => None
        case (Empty, c2 @ Completed(_)) => Some(c2)
        case (Completed(a1) , Completed(a2)) =>
          if(EqA.equal(a1, a2)) None
          else Some(Conflict)
        case (Conflict, _) => None
        case (_, Conflict) => Some(Conflict)
      }

      override def appendDeltas(d1: Unit, d2: Unit): Unit = ()
    }

  implicit def equalInstance[A: Equal]: Equal[Promise[A]] = new Equal[Promise[A]] {
    def equal(p1: Promise[A], p2: Promise[A]): Boolean = (p1, p2) match {
      case (Completed(a1), Completed(a2)) => a1 === a2
      case (Empty, Empty) => true
      case (Conflict, Conflict) => true
      case _ => false
    }
  }

  implicit def deepEqualInstance[A, B, Ptr1[_], Ptr2[_]](implicit ev: DeepEqual[A, B, Ptr1, Ptr2]): DeepEqual[Promise[A], Promise[B], Ptr1, Ptr2] =
    new DeepEqual[Promise[A], Promise[B], Ptr1, Ptr2] {
      def equal(p1: Promise[A], p2: Promise[B]): IsEqual[Ptr1, Ptr2] = (p1, p2) match {
        case (Completed(a1), Completed(a2)) => ev.equal(a1, a2)
        case (Empty, Empty) => IsEqual(true)
        case (Conflict, Conflict) => IsEqual(true)
        case _ => IsEqual(false)
      }
    }

  implicit def deepShowInstance[A, Ptr[_]](implicit ev: DeepShow[A, Ptr]): DeepShow[Promise[A], Ptr] =
    new DeepShow.FromSerialize[Promise[A], Ptr] {
      def serialize[M[_]](a: Promise[A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] = a match {
        case Empty => M.write("?")
        case Conflict => M.write("⊥")
        case Completed(a) => ev.serialize(a)
      }
    }
}