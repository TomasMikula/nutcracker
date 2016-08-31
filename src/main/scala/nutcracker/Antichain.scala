package nutcracker

import nutcracker.Dom.{Refined, Status}
import nutcracker.util.{ContF, InjectK, Uninhabited}

import scala.language.higherKinds
import scalaz.Show

/** Marker wrapper meaning that any two distinct values of type `Antichain[A]`
  * should be treated as incomparable. As a consequence, a value of type
  * `Antichain[A]` cannot be refined to obtain a different value. Therefore,
  * refinement is disallowed altogether by setting the `Update` type to
  * an uninhabited type.
  *
  * @see [[Promise]]
  */
final case class Antichain[A](value: A) extends AnyVal

object Antichain {

  type Update[A] = Uninhabited
  type Delta[A] = Uninhabited

  type Ref[A] = DRef.Aux[Antichain[A], Update[A], Delta[A]]

  def map[F[_[_], _], A, B](refC: ContF[F, Ref[A]])(f: A => B)(implicit i: InjectK[PropagationLang, F]): ContF[F, Ref[B]] = for {
    ref <- refC
    a   <- ref.asCont[F]
    res <- cellC(f(a))
  } yield res

  def mapC[F[_[_], _], A, B](ref: Ref[A])(f: A => ContF[F, Ref[B]])(implicit i: InjectK[PropagationLang, F]): ContF[F, Ref[B]] = for {
    a   <- ref.asCont[F]
    res <- f(a)
  } yield res

  def filterMap[F[_[_], _], A, B](refC: ContF[F, Ref[A]])(f: A => Option[B])(implicit i: InjectK[PropagationLang, F]): ContF[F, Ref[B]] = for {
    ref <- refC
    a   <- ref.asCont[F]
    res <- f(a) match {
      case Some(b) => ContF.liftM(PropagationLang.cellF(Antichain(b)).inject[F])
      case None    => ContF.noop[F, Ref[B]]
    }
  } yield res

  def cellC[F[_[_], _], A](a: A)(implicit i: InjectK[PropagationLang, F]): ContF[F, Ref[A]] =
    ContF.liftM(PropagationLang.cellF(Antichain(a)).inject[F])

  implicit def domInstance[A]: Dom.Aux[Antichain[A], Update[A], Delta[A]] = new Dom[Antichain[A]] {
    type Update = Antichain.Update[A]
    type Delta = Antichain.Delta[A]

    def update(d: Antichain[A], u: Update): Option[(Antichain[A], Delta)] = None
    def combineDeltas(d1: Delta, d2: Delta): Delta = sys.error("unreachable code")
    def assess(d: Antichain[A]): Status[Update] = Refined
  }

  implicit def finalInstance[A]: Final.Aux[Antichain[A], A] = new Final[Antichain[A]] {
    type Out = A

    def extract(a: Antichain[A]): Option[A] = Some(a.value)
  }

  implicit def showInstance[A](implicit A: Show[A]): Show[Antichain[A]] = new Show[Antichain[A]] {
    override def shows(a: Antichain[A]): String = A.shows(a.value)
  }

}