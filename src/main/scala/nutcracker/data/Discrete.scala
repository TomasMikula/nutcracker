package nutcracker.data

import nutcracker.ops.Ops._
import nutcracker.util.{ContU, DeepEqual, DeepShow, IsEqual, MonadObjectOutput, Uninhabited}
import nutcracker.{Final, Propagation, RDom, UpdateResult}
import scalaz.Id.Id
import scalaz.{Bind, ContT, Functor, Monad, Show}
import scalaz.syntax.contravariant._

/** Marker wrapper meaning that any two distinct values of type `Discrete[A]`
  * should be treated as incomparable. As a consequence, a value of type
  * `Discrete[A]` cannot be refined to obtain a different value. Therefore,
  * refinement is disallowed altogether by setting the `Update` type to
  * an uninhabited type.
  *
  * @see [[Revocable]] which in addition has a top element.
  * @see [[Promise]] which in addition has bottom and top elements.
  */
final case class Discrete[A](value: A) extends AnyVal

object Discrete extends DiscreteInstances {

  type Update[A] = Uninhabited
  type Delta[A] = Uninhabited

  def map[M[_], Var[_], Val[_], A, B](refC: ContU[M, Var[Discrete[A]]])(f: A => B)(implicit M: Propagation[M, Var, Val], MB: Bind[M]): ContU[M, Var[Discrete[B]]] = for {
    ref <- refC
    a   <- ref.asCont_
    res <- cellC(f(a))
  } yield res

  def mapC[M[_], Var[_], Val[_], A, B](ref: Var[Discrete[A]])(f: A => ContU[M, Var[Discrete[B]]])(implicit P: Propagation[M, Var, Val], M: Functor[M]): ContU[M, Var[Discrete[B]]] = for {
    a   <- ref.asCont_
    res <- f(a)
  } yield res

  def filterMap[M[_], Var[_], Val[_], A, B](refC: ContT[Unit, M, Var[Discrete[A]]])(f: A => Option[B])(implicit P: Propagation[M, Var, Val], M: Monad[M]): ContT[Unit, M, Var[Discrete[B]]] = for {
    ref <- refC
    a   <- ref.asCont_
    res <- f(a) match {
      case Some(b) => ContU.liftM(P.newCell(Discrete(b)))
      case None    => ContU.noop[M, Var[Discrete[B]]]
    }
  } yield res

  def cellC[M[_], Var[_], Val[_], A](a: A)(implicit M: Propagation[M, Var, Val], MB: Bind[M]): ContT[Unit, M, Var[Discrete[A]]] =
    ContT.liftM[Id, Unit, M, Var[Discrete[A]]](M.newCell(Discrete(a)))

  implicit def domInstance[A]: RDom.Aux[Discrete[A], Update[A], Delta[A]] = new RDom[Discrete[A]] {
    type Update = Discrete.Update[A]
    type Delta = Discrete.Delta[A]

    def update[D <: Discrete[A]](d: D, u: Update): UpdateResult[Discrete[A], IDelta, D] = sys.error("unreachable code")
    def appendDeltas(d1: Delta, d2: Delta): Delta = sys.error("unreachable code")
    def isFailed(d: Discrete[A]): Boolean = false
    def recur(δ: Delta): Update = δ
  }

  implicit def finalInstance[A]: Final.Aux[Discrete[A], A] = new Final[Discrete[A]] {
    type Out = A

    def extract(a: Discrete[A]): Option[A] = Some(a.value)

    def embed(a: A): Discrete[A] = Discrete(a)
  }

  implicit def showInstance[A](implicit A: Show[A]): Show[Discrete[A]] =
    A.contramap(_.value)

}

trait DiscreteInstances extends DiscreteInstances1 {

  implicit def deepEqualInstance1[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Discrete[A1], A2, Ptr1, Ptr2] =
    new DeepEqual[Discrete[A1], A2, Ptr1, Ptr2] {
      def equal(a1: Discrete[A1], a2: A2): IsEqual[Ptr1, Ptr2] = ev.equal(a1.value, a2)
    }

  implicit def deepShowInstance[Ptr[_], A](implicit ev: DeepShow[A, Ptr]): DeepShow[Discrete[A], Ptr] =
    new DeepShow.FromSerialize[Discrete[A], Ptr] {
      def serialize[M[_]](a: Discrete[A])(implicit M: MonadObjectOutput[M, String, Ptr]): M[Unit] =
        ev.serialize(a.value)
    }

}

trait DiscreteInstances1 {

  implicit def deepEqualInstance2[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[A1, Discrete[A2], Ptr1, Ptr2] =
    new DeepEqual[A1, Discrete[A2], Ptr1, Ptr2] {
      def equal(a1: A1, a2: Discrete[A2]): IsEqual[Ptr1, Ptr2] = ev.equal(a1, a2.value)
    }

}