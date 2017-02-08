package nutcracker.util

import nutcracker.util.free.Free

import scalaz.{Applicative, BindRec, IList, INil, -\/, \/, \/-, ~>}
import scalaz.syntax.monad._

final case class IsEqual[Ptr1[_], Ptr2[_]] private (private val unwrap: Free[IsEqual.IsEqF[Ptr1, Ptr2, ?], Boolean]) { // extends AnyVal // https://issues.scala-lang.org/browse/SI-7685
  import IsEqual._

  def &&(that: => IsEqual[Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = IsEqual(unwrap.flatMap(if(_) that.unwrap else IsEqual(false).unwrap))
  def ||(that: => IsEqual[Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = IsEqual(unwrap.flatMap(if(_) IsEqual(true).unwrap else that.unwrap))

  // XXX: Relies on meaningful hashCode and equals for Ptr1
  def eval[M[_]](deref1: Ptr1 ~> M, deref2: Ptr2 ~> M)(implicit eq2: HEqualK[Ptr2], M0: BindRec[M], M1: Applicative[M]): M[Boolean] = {
    type Γ = KMap[Ptr1, λ[α => IList[Exists[Ptr2]]]]
    def Γ(): Γ = KMap[Ptr1, λ[α => IList[Exists[Ptr2]]]]()
    import scalaz.std.anyVal._ // need Monoid[Unit]

    type F[X] = IsEqF[Ptr1, Ptr2, X]

    M0.map(unwrap.foldRunRecParM[M, Γ, Unit](Γ(), λ[λ[α => (Γ, F[α])] ~> λ[α => M[(Γ, Free[F, α], Unit => Unit) \/ (Unit, α)]]] {
      case (γ, fx) => fx match {
        case Pair(p1, p2, f) =>
          val p1_eq = γ.getOrElse(p1)(INil())
          if(p1_eq.find(ep2 => eq2.hEqual(ep2.value, p2)).isDefined) M1.point(\/-(((), true)))
          else deref1(p1) >>= (x => M1.map(deref2(p2))(y => -\/((γ.put(p1)(Exists(p2) :: p1_eq), f(x, y).unwrap, identity))))
      }
    }))(_._2)
  }
}

object IsEqual {
  private[util] sealed abstract class IsEqF[Ptr1[_], Ptr2[_], A]
  private[util] case class Pair[Ptr1[_], Ptr2[_], X, Y](p1: Ptr1[X], p2: Ptr2[Y], f: (X, Y) => IsEqual[Ptr1, Ptr2]) extends IsEqF[Ptr1, Ptr2, Boolean]

  def apply[Ptr1[_], Ptr2[_]](value: Boolean): IsEqual[Ptr1, Ptr2] =
    IsEqual(Free.point[IsEqF[Ptr1, Ptr2, ?], Boolean](value))

  def apply[Ptr1[_], Ptr2[_], A1, A2](a1: A1, a2: A2)(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    ev.equal(a1, a2)

  def refs[Ptr1[_], Ptr2[_], X, Y](p1: Ptr1[X], p2: Ptr2[Y])(implicit ev: DeepEqual[X, Y, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    IsEqual(Free.liftF[IsEqF[Ptr1, Ptr2, ?], Boolean](Pair(p1, p2, ev.equal)))

  def apply[Ptr1[_], Ptr2[_]]: PApplied[Ptr1, Ptr2] = new PApplied[Ptr1, Ptr2]
  final class PApplied[Ptr1[_], Ptr2[_]] private[IsEqual] {
    def equal[A1, A2](a1: A1, a2: A2)(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = ev.equal(a1, a2)
  }

  def optionEqual[Ptr1[_], Ptr2[_], A1, A2](o1: Option[A1], o2: Option[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    (o1, o2) match {
      case (Some(a1), Some(a2)) => ev.equal(a1, a2)
      case (None, None) => IsEqual(true)
      case _ => IsEqual(false)
    }

  def eitherEqual[Ptr1[_], Ptr2[_], A1, B1, A2, B2](e1: Either[A1, B1], e2: Either[A2, B2])(implicit eva: DeepEqual[A1, A2, Ptr1, Ptr2], evb: DeepEqual[B1, B2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    (e1, e2) match {
      case (Left(a1), Left(a2)) => eva.equal(a1, a2)
      case (Right(b1), Right(b2)) => evb.equal(b1, b2)
      case _ => IsEqual(false)
    }

  def listEqual[Ptr1[_], Ptr2[_], A1, A2](l1: List[A1], l2: List[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    if(l1.size != l2.size) IsEqual(false)
    else {
      def go(l1: List[A1], l2: List[A2]): IsEqual[Ptr1, Ptr2] = (l1, l2) match {
        case (a :: as, b :: bs) => ev.equal(a, b) && go(as, bs)
        case _ => IsEqual(true)
      }
      go(l1, l2)
    }

  def vectorEqual[Ptr1[_], Ptr2[_], A1, A2](v1: Vector[A1], v2: Vector[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    if(v1.size != v2.size) IsEqual(false)
    else {
      def go(i: Int): IsEqual[Ptr1, Ptr2] =
        if(i >= 0) ev.equal(v1(i), v2(i)) && go(i-1)
        else IsEqual(true)

      go(v1.size - 1)
    }

  def unorderedListEqual[Ptr1[_], Ptr2[_], A1, A2](l1: List[A1], l2: List[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    if(l1.size != l2.size) IsEqual(false)
    else {
      def proceed(l1: List[A1], l2: List[A2]): IsEqual[Ptr1, Ptr2] = {
        assert(l1.size == l2.size)
        l1 match {
          case h :: t => locate(h, t, Nil, l2)
          case Nil => IsEqual(true)
        }
      }

      // finds the first occurrence of h1 in r2 and then proceeds with equating t1 with (l2 ++ (r2 - h1))
      def locate(h1: A1, t1: List[A1], l2: List[A2], r2: List[A2]): IsEqual[Ptr1, Ptr2] = {
        assert(1 + t1.size == l2.size + r2.size)
        r2 match {
          case e2 :: r2 => ev.equal(h1, e2) && proceed(t1, l2 reverse_::: r2) || locate(h1, t1, e2 :: l2, r2)
          case Nil => IsEqual(false)
        }

      }

      proceed(l1, l2)
    }

  def setEqual[Ptr1[_], Ptr2[_], A1, A2](s1: Set[A1], s2: Set[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    unorderedListEqual(s1.toList, s2.toList)

  def tuple2[Ptr1[_], Ptr2[_], A1, B1, A2, B2](t1: (A1, B1), t2: (A2, B2))(implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2]
  ): IsEqual[Ptr1, Ptr2] =
    eva.equal(t1._1, t2._1) && evb.equal(t1._2, t2._2)

  def tuple3[Ptr1[_], Ptr2[_], A1, B1, C1, A2, B2, C2](t1: (A1, B1, C1), t2: (A2, B2, C2))(implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2]
  ): IsEqual[Ptr1, Ptr2] =
    eva.equal(t1._1, t2._1) && evb.equal(t1._2, t2._2) && evc.equal(t1._3, t2._3)

  def tuple4[Ptr1[_], Ptr2[_], A1, B1, C1, D1, A2, B2, C2, D2](t1: (A1, B1, C1, D1), t2: (A2, B2, C2, D2))(implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2],
    evd: DeepEqual[D1, D2, Ptr1, Ptr2]
  ): IsEqual[Ptr1, Ptr2] =
    eva.equal(t1._1, t2._1) && evb.equal(t1._2, t2._2) && evc.equal(t1._3, t2._3) && evd.equal(t1._4, t2._4)

  def tuple5[Ptr1[_], Ptr2[_], A1, B1, C1, D1, E1, A2, B2, C2, D2, E2](t1: (A1, B1, C1, D1, E1), t2: (A2, B2, C2, D2, E2))(implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2],
    evd: DeepEqual[D1, D2, Ptr1, Ptr2],
    eve: DeepEqual[E1, E2, Ptr1, Ptr2]
  ): IsEqual[Ptr1, Ptr2] =
    eva.equal(t1._1, t2._1) && evb.equal(t1._2, t2._2) && evc.equal(t1._3, t2._3) && evd.equal(t1._4, t2._4) && eve.equal(t1._5, t2._5)

  def tuple6[Ptr1[_], Ptr2[_], A1, B1, C1, D1, E1, F1, A2, B2, C2, D2, E2, F2](t1: (A1, B1, C1, D1, E1, F1), t2: (A2, B2, C2, D2, E2, F2))(implicit
    eva: DeepEqual[A1, A2, Ptr1, Ptr2],
    evb: DeepEqual[B1, B2, Ptr1, Ptr2],
    evc: DeepEqual[C1, C2, Ptr1, Ptr2],
    evd: DeepEqual[D1, D2, Ptr1, Ptr2],
    eve: DeepEqual[E1, E2, Ptr1, Ptr2],
    evf: DeepEqual[F1, F2, Ptr1, Ptr2]
  ): IsEqual[Ptr1, Ptr2] =
    eva.equal(t1._1, t2._1) && evb.equal(t1._2, t2._2) && evc.equal(t1._3, t2._3) && evd.equal(t1._4, t2._4) && eve.equal(t1._5, t2._5) && evf.equal(t1._6, t2._6)

}