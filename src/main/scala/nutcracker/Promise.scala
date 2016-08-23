package nutcracker

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

  implicit def embedInstance[A]: Embed[A, Promise[A]] = new Embed[A, Promise[A]] {
    def embed(a: A): Promise[A] = Completed(a)
  }

  implicit def extractInstance[A]: Extract.Aux[Promise[A], A] = new Extract[Promise[A]] {
    type Out = A

    def extract(pa: Promise[A]): Option[A] = pa match {
      case Completed(a) => Some(a)
      case _ => None
    }
  }

  implicit def promiseDomain[A]: Dom.Aux[Promise[A], Complete[A], Unit] = new Dom[Promise[A]] {
    type Update = Complete[A]
    type Delta = Unit

    override def assess(pa: Promise[A]): Dom.Status[Complete[A]] = pa match {
      case Empty => Dom.Unrefined(() => None)
      case Completed(a) => Dom.Refined
      case Contradiction => Dom.Failed
    }

    /** Completed promise cannot be completed again. Therefore, any attempt
      * to refine a completed promise will result in conflict, even if refining
      * with the exact same value. Note that this breaks the idempotence of
      * updates, but it allows us to not require `Eq` instance on `A`.
      */
    override def update(p: Promise[A], v: Complete[A]): Option[(Promise[A], Unit)] = p match {
      case Empty => Some((Completed(v.value), ()))
      case Completed(_) => Some((Contradiction, ()))
      case Contradiction => None
    }

    override def combineDeltas(d1: Unit, d2: Unit): Unit = ()
  }
}