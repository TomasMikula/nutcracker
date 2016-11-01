package nutcracker

import nutcracker.Dom.{Refined, Status}
import nutcracker.util.{ContU, Uninhabited}

import scala.language.higherKinds
import scalaz.{Bind, ContT, Monad, Show}
import scalaz.Id.Id

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

  def map[M[_], Ref[_], A, B](refC: ContU[M, Ref[Antichain[A]]])(f: A => B)(implicit M: Propagation[M, Ref], MB: Bind[M]): ContU[M, Ref[Antichain[B]]] = for {
    ref <- refC
    a   <- ref.asCont[M]
    res <- cellC(f(a))
  } yield res

  def mapC[M[_], Ref[_]: Propagation[M, ?[_]], A, B](ref: Ref[Antichain[A]])(f: A => ContU[M, Ref[Antichain[B]]]): ContU[M, Ref[Antichain[B]]] = for {
    a   <- ref.asCont[M]
    res <- f(a)
  } yield res

  def filterMap[M[_], Ref[_], A, B](refC: ContT[M, Unit, Ref[Antichain[A]]])(f: A => Option[B])(implicit P: Propagation[M, Ref], M: Monad[M]): ContT[M, Unit, Ref[Antichain[B]]] = for {
    ref <- refC
    a   <- ref.asCont[M]
    res <- f(a) match {
      case Some(b) => ContU.liftM(P.cell(Antichain(b)))
      case None    => ContU.noop[M, Ref[Antichain[B]]]
    }
  } yield res

  def cellC[M[_], Ref[_], A](a: A)(implicit M: Propagation[M, Ref], MB: Bind[M]): ContT[M, Unit, Ref[Antichain[A]]] =
    ContT.liftM[Id, M, Unit, Ref[Antichain[A]]](M.cell(Antichain(a)))

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

    def embed(a: A): Antichain[A] = Antichain(a)
  }

  implicit def showInstance[A](implicit A: Show[A]): Show[Antichain[A]] = new Show[Antichain[A]] {
    override def shows(a: Antichain[A]): String = A.shows(a.value)
  }

}