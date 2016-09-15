package nutcracker

import scalaz.Equal

/** For any type `A`, `Promise[A]` is a bounded lattice on the set `A ⊔ {0, 1}`
  * (where "⊔" means "disjoint union") with `0` being the least element, `1`
  * being the greatest element, and all elements of `A` forming an antichain
  * (i.e. mutually incomparable). `0` represents an empty (incomplete) promise,
  * the elements of `A` represent a completed promise, and `1` represents a
  * conflict (a promise completed multiple times with different values).
  *
  * @see [[Antichain]]
  */
sealed trait Promise[+A]

object Promise {

  final case object Empty extends Promise[Nothing] // incomplete promise
  final case class Completed[A](value: A) extends Promise[A]
  final case object Conflict extends Promise[Nothing] // promise completed multiple times with different values

  final case class Complete[A](value: A) extends AnyVal

  type Ref[A] = DRef.Aux[Promise[A], Complete[A], Unit]

  def empty[A]: Promise[A] = Empty
  def completed[A](a: A): Promise[A] = Completed(a)

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

    override def update(p: Promise[A], v: Complete[A]): Option[(Promise[A], Unit)] = p match {
      case Empty => Some((Completed(v.value), ()))
      case Completed(a) =>
        if(EqA.equal(a, v.value)) None
        else Some((Conflict, ()))
      case Conflict => None
    }

    override def ljoin(d1: Promise[A], d2: Promise[A]): Option[(Promise[A], Unit)] = (d1, d2) match {
      case (_, Empty) => None
      case (d1, Completed(a)) => update(d1, Complete(a))
      case (Conflict, Conflict) => None
      case (_, Conflict) => Some((Conflict, ()))
    }

    override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
  }
}