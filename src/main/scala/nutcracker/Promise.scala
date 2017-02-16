package nutcracker

import scala.language.higherKinds
import nutcracker.Promise.Empty
import nutcracker.util.{DeepEqual, DeepShow, IsEqual, MonadObjectOutput}

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
  def isEmpty: Boolean = this == Empty
  def nonEmpty: Boolean = this != Empty
}

object Promise {

  final case object Empty extends Promise[Nothing] // incomplete promise
  final case class Completed[A](value: A) extends Promise[A]
  final case object Conflict extends Promise[Nothing] // promise completed multiple times with different values

  final case class Complete[A](value: A) extends AnyVal

  type Update[A] = Complete[A]
  type Delta[A] = Unit

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

  implicit def promiseDomain[A](implicit EqA: Equal[A]): JoinDom.Aux[Promise[A], Complete[A], Unit] = new JoinDom[Promise[A]] {
    type Update = Complete[A]
    type Delta = Unit

    override def assess(pa: Promise[A]): Dom.Status[Complete[A]] = pa match {
      case Empty => Dom.Unrefined(() => None)
      case Completed(a) => Dom.Refined
      case Conflict => Dom.Failed
    }

    override def update[P <: Promise[A]](p: P, v: Complete[A]): UpdateResult[Promise[A], IDelta, P] = p match {
      case Empty => UpdateResult(Completed(v.value), ())
      case Completed(a) =>
        if(EqA.equal(a, v.value)) UpdateResult()
        else UpdateResult(Conflict, ())
      case Conflict => UpdateResult()
    }

    override def ljoin[P <: Promise[A]](d1: P, d2: Promise[A]): UpdateResult[Promise[A], IDelta, P] = (d1, d2) match {
      case (_, Empty) => UpdateResult()
      case (d1, Completed(a)) => update(d1, Complete(a))
      case (Conflict, Conflict) => UpdateResult()
      case (_, Conflict) => UpdateResult(Conflict, ())
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