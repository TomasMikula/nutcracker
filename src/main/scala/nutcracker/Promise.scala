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

  private final case object Empty extends Promise[Nothing] // incomplete promise
  private final case class Completed[A](value: A) extends Promise[A]
  private final case object Contradiction extends Promise[Nothing] // promise completed multiple times

  final case class Complete[A](value: A) extends AnyVal

  type Ref[A] = DRef.Aux[Promise[A], Complete[A], Unit]

  def empty[A]: Promise[A] = Empty

  implicit def finalInstance[A]: Final.Aux[Promise[A], A] = new Final[Promise[A]] {
    type Out = A

    def extract(pa: Promise[A]): Option[A] = pa match {
      case Completed(a) => Some(a)
      case _ => None
    }

    def embed(a: A): Promise[A] = Completed(a)
  }

  implicit def promiseDomain[A](implicit EqA: Equal[A]): Dom.Aux[Promise[A], Complete[A], Unit] = new Dom[Promise[A]] {
    type Update = Complete[A]
    type Delta = Unit

    override def assess(pa: Promise[A]): Dom.Status[Complete[A]] = pa match {
      case Empty => Dom.Unrefined(() => None)
      case Completed(a) => Dom.Refined
      case Contradiction => Dom.Failed
    }

    override def update(p: Promise[A], v: Complete[A]): Option[(Promise[A], Unit)] = p match {
      case Empty => Some((Completed(v.value), ()))
      case Completed(a) =>
        if(EqA.equal(a, v.value)) None
        else Some((Contradiction, ()))
      case Contradiction => None
    }

    override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
  }
}