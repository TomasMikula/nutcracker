package nutcracker.util

import scalaz.{Applicative, BindRec, ~>}

/** Comparing of (potentially cyclic) object graphs for equality.
  * Features:
  *  - abstracted over pointers;
  *  - termination and correctness in presence of cycles;
  *  - stack safety.
  *
  * XXX: Relies on meaningful hashCode and equals for Ptr1
  * @tparam A1
  * @tparam A2
  * @tparam Ptr1
  * @tparam Ptr2
  */
trait DeepEqual[A1, A2, Ptr1[_], Ptr2[_]] {

  def equal(a1: A1, a2: A2): IsEqual[Ptr1, Ptr2]

  final def deepEqual[M[_]](a1: A1, a2: A2)(deref1: Ptr1 ~> M, deref2: Ptr2 ~> M)(implicit eq2: HEqualK[Ptr2], M0: BindRec[M], M1: Applicative[M]): M[Boolean] =
    equal(a1, a2).eval(deref1, deref2)

  final def lift: DeepEqual[Ptr1[A1], Ptr2[A2], Ptr1, Ptr2] =
    new DeepEqual[Ptr1[A1], Ptr2[A2], Ptr1, Ptr2] {
      def equal(p1: Ptr1[A1], p2: Ptr2[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.refs(p1, p2)(DeepEqual.this)
    }
}

object DeepEqual {

  implicit def lift[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Ptr1[A1], Ptr2[A2], Ptr1, Ptr2] =
    ev.lift

  implicit def specialize[Ptr1[_], Ptr2[_], A1[_[_]], A2[_[_]]](implicit ev: DeepEqualK[A1, A2]): DeepEqual[A1[Ptr1], A2[Ptr2], Ptr1, Ptr2] =
    ev.specialize

  implicit def intInstance[Ptr1[_], Ptr2[_]]: DeepEqual[Int, Int, Ptr1, Ptr2] = new DeepEqual[Int, Int, Ptr1, Ptr2] {
    def equal(a1: Int, a2: Int): IsEqual[Ptr1, Ptr2] = IsEqual(a1 == a2)
  }

  implicit def optionInstance[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Option[A1], Option[A2], Ptr1, Ptr2] =
    new DeepEqual[Option[A1], Option[A2], Ptr1, Ptr2] {
      def equal(a1: Option[A1], a2: Option[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.optionEqual(a1, a2)
    }

  implicit def eitherInstance[Ptr1[_], Ptr2[_], A1, B1, A2, B2](implicit eva: DeepEqual[A1, A2, Ptr1, Ptr2], evb: DeepEqual[B1, B2, Ptr1, Ptr2]): DeepEqual[Either[A1, B1], Either[A2, B2], Ptr1, Ptr2] =
    new DeepEqual[Either[A1, B1], Either[A2, B2], Ptr1, Ptr2] {
      def equal(a1: Either[A1, B1], a2: Either[A2, B2]): IsEqual[Ptr1, Ptr2] = IsEqual.eitherEqual(a1, a2)
    }

  implicit def listInstance[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[List[A1], List[A2], Ptr1, Ptr2] =
    new DeepEqual[List[A1], List[A2], Ptr1, Ptr2] {
      def equal(a1: List[A1], a2: List[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.listEqual(a1, a2)
    }

  implicit def vectorInstance[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Vector[A1], Vector[A2], Ptr1, Ptr2] =
    new DeepEqual[Vector[A1], Vector[A2], Ptr1, Ptr2] {
      def equal(a1: Vector[A1], a2: Vector[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.vectorEqual(a1, a2)
    }

  implicit def setInstance[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Set[A1], Set[A2], Ptr1, Ptr2] =
    new DeepEqual[Set[A1], Set[A2], Ptr1, Ptr2] {
      def equal(a1: Set[A1], a2: Set[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.setEqual(a1, a2)
    }

  implicit def tuple2Instance[Ptr1[_], Ptr2[_], A1, B1, A2, B2](implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2]
  ): DeepEqual[(A1, B1), (A2, B2), Ptr1, Ptr2] =
    new DeepEqual[(A1, B1), (A2, B2), Ptr1, Ptr2] {
      def equal(a1: (A1, B1), a2: (A2, B2)): IsEqual[Ptr1, Ptr2] = IsEqual.tuple2(a1, a2)
    }

  implicit def tuple3Instance[Ptr1[_], Ptr2[_], A1, B1, C1, A2, B2, C2](implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2]
  ): DeepEqual[(A1, B1, C1), (A2, B2, C2), Ptr1, Ptr2] =
    new DeepEqual[(A1, B1, C1), (A2, B2, C2), Ptr1, Ptr2] {
      def equal(a1: (A1, B1, C1), a2: (A2, B2, C2)): IsEqual[Ptr1, Ptr2] = IsEqual.tuple3(a1, a2)
    }

  implicit def tuple4Instance[Ptr1[_], Ptr2[_], A1, B1, C1, D1, A2, B2, C2, D2](implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2],
    evd: DeepEqual[D1, D2, Ptr1, Ptr2]
  ): DeepEqual[(A1, B1, C1, D1), (A2, B2, C2, D2), Ptr1, Ptr2] =
    new DeepEqual[(A1, B1, C1, D1), (A2, B2, C2, D2), Ptr1, Ptr2] {
      def equal(a1: (A1, B1, C1, D1), a2: (A2, B2, C2, D2)): IsEqual[Ptr1, Ptr2] = IsEqual.tuple4(a1, a2)
    }

  implicit def tuple5Instance[Ptr1[_], Ptr2[_], A1, B1, C1, D1, E1, A2, B2, C2, D2, E2](implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2],
    evd: DeepEqual[D1, D2, Ptr1, Ptr2],
    eve: DeepEqual[E1, E2, Ptr1, Ptr2]
  ): DeepEqual[(A1, B1, C1, D1, E1), (A2, B2, C2, D2, E2), Ptr1, Ptr2] =
    new DeepEqual[(A1, B1, C1, D1, E1), (A2, B2, C2, D2, E2), Ptr1, Ptr2] {
      def equal(a1: (A1, B1, C1, D1, E1), a2: (A2, B2, C2, D2, E2)): IsEqual[Ptr1, Ptr2] = IsEqual.tuple5(a1, a2)
    }

  implicit def tuple6Instance[Ptr1[_], Ptr2[_], A1, B1, C1, D1, E1, F1, A2, B2, C2, D2, E2, F2](implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2],
    evd: DeepEqual[D1, D2, Ptr1, Ptr2],
    eve: DeepEqual[E1, E2, Ptr1, Ptr2],
    evf: DeepEqual[F1, F2, Ptr1, Ptr2]
  ): DeepEqual[(A1, B1, C1, D1, E1, F1), (A2, B2, C2, D2, E2, F2), Ptr1, Ptr2] =
    new DeepEqual[(A1, B1, C1, D1, E1, F1), (A2, B2, C2, D2, E2, F2), Ptr1, Ptr2] {
      def equal(a1: (A1, B1, C1, D1, E1, F1), a2: (A2, B2, C2, D2, E2, F2)): IsEqual[Ptr1, Ptr2] = IsEqual.tuple6(a1, a2)
    }
}

trait DeepEqualK[F1[_[_]], F2[_[_]]] {
  def equal[Ptr1[_], Ptr2[_]](f1: F1[Ptr1], f2: F2[Ptr2]): IsEqual[Ptr1, Ptr2]

  def specialize[Ptr1[_], Ptr2[_]]: DeepEqual[F1[Ptr1], F2[Ptr2], Ptr1, Ptr2] =
    new DeepEqual[F1[Ptr1], F2[Ptr2], Ptr1, Ptr2] {
      def equal(f1: F1[Ptr1], f2: F2[Ptr2]): IsEqual[Ptr1, Ptr2] = DeepEqualK.this.equal(f1, f2)
    }
}



