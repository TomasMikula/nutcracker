package nutcracker

sealed trait Promise[+A]

object Promise {

  private final case object Empty extends Promise[Nothing] // incomplete promise
  private final case class Completed[A](value: A) extends Promise[A]
  private final case object Contradiction extends Promise[Nothing] // promise completed multiple times

  final case class Complete[A](value: A) extends AnyVal

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

  implicit def promiseDomain[A]: Dom[Promise[A], Complete[A], Unit] = new Dom[Promise[A], Complete[A], Unit] {
    override def assess(pa: Promise[A]): Dom.Status[Complete[A]] = pa match {
      case Empty => Dom.Unrefined(() => None)
      case Completed(a) => Dom.Refined
      case Contradiction => Dom.Failed
    }

    /** Completed promise cannot be completed again. Therefore, any attempt
      * to refine a completed promise will result in bottom, even if refining
      * with the exact same value. Note that this breaks the idempotence of
      * updates, but it allows us to not require `Eq` instance on `A`.
      */
    override def update(p: Promise[A], v: Complete[A]): Option[(Promise[A], Unit)] = p match {
      case Empty => Some((Completed(v.value), ()))
      case Completed(_) => Some((Contradiction, ()))
      case Contradiction => None
    }

    def combineDiffs(d1: Unit, d2: Unit): Unit = ()
  }
}