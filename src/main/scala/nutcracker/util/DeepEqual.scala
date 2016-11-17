package nutcracker.util

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.Id._
import scalaz.{ICons, IList, INil, ~>}

/**
  * XXX: Relies on meaningful hashCode and equal for Ptr1
  * @tparam A1
  * @tparam A2
  * @tparam Ptr1
  * @tparam Ptr2
  */
trait DeepEqual[A1, A2, Ptr1[_], Ptr2[_]] {

  def equal(a1: A1, a2: A2): IsEqual[Ptr1, Ptr2]

  final def deepEqual(a1: A1, a2: A2)(deref1: Ptr1 ~> Id, deref2: Ptr2 ~> Id)(implicit eq2: HEqualK[Ptr2]): Boolean =
    DeepEqual.deepEqual(equal(a1, a2))(deref1, deref2)

  final def lift: DeepEqual[Ptr1[A1], Ptr2[A2], Ptr1, Ptr2] =
    new DeepEqual[Ptr1[A1], Ptr2[A2], Ptr1, Ptr2] {
      def equal(p1: Ptr1[A1], p2: Ptr2[A2]): IsEqual[Ptr1, Ptr2] = IsEqual(p1, p2)(DeepEqual.this)
    }
}

object DeepEqual {
  private[DeepEqual] type Γ[Ptr1[_], Ptr2[_]] = KMap[Ptr1, λ[α => IList[Exists[Ptr2]]]]
  private[DeepEqual] object Γ {
    def apply[Ptr1[_], Ptr2[_]](): Γ[Ptr1, Ptr2] = KMap[Ptr1, λ[α => IList[Exists[Ptr2]]]]()
  }

  private[DeepEqual] type StackFrame[Ptr1[_], Ptr2[_]] =
    Boolean => (IsEqual[Ptr1, Ptr2], Γ[Ptr1, Ptr2])

  private[DeepEqual] def deepEqual[Ptr1[_], Ptr2[_]](
    e: IsEqual[Ptr1, Ptr2])(
    deref1: Ptr1 ~> Id,
    deref2: Ptr2 ~> Id
  )(implicit
    eq2: HEqualK[Ptr2]
  ): Boolean =
    deepEqual(e, Γ[Ptr1, Ptr2](), INil[StackFrame[Ptr1, Ptr2]]())(deref1, deref2)

  @tailrec
  private[DeepEqual] def deepEqual[Ptr1[_], Ptr2[_]](
    e: IsEqual[Ptr1, Ptr2],
    γ: Γ[Ptr1, Ptr2],
    stack: IList[StackFrame[Ptr1, Ptr2]])(
    deref1: Ptr1 ~> Id,
    deref2: Ptr2 ~> Id
  )(implicit
    eq2: HEqualK[Ptr2]
  ): Boolean = {
    import IsEqual._

    e match {
      case Const(v) =>
        stack match {
          case ICons(f, fs) => val (e1, γ1) = f(v); deepEqual(e1, γ1, fs)(deref1, deref2)
          case INil() => v
        }
      case Indirect(p1, p2, ev) =>
        val p1_eq = γ.getOrElse(p1)(INil())
        if(p1_eq.find(ep2 => eq2.hEqual(ep2.value, p2)).isDefined) deepEqual(Const(true), γ, stack)(deref1, deref2)
        else deepEqual(ev.equal(deref1(p1), deref2(p2)), γ.put(p1)(Exists(p2) :: p1_eq), stack)(deref1, deref2)
      case And(e1, e2) =>
        val cont: StackFrame[Ptr1, Ptr2] = if(_) (e2(), γ) else (Const(false), γ)
        deepEqual(e1, γ, cont :: stack)(deref1, deref2)
      case Or(e1, e2) =>
        val cont: StackFrame[Ptr1, Ptr2] = if(_) (Const(true), γ) else (e2(), γ)
        deepEqual(e1, γ, cont :: stack)(deref1, deref2)
    }
  }

  implicit def lift[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Ptr1[A1], Ptr2[A2], Ptr1, Ptr2] =
    ev.lift

  implicit def specialize[Ptr1[_], Ptr2[_], A1[_[_]], A2[_[_]]](implicit ev: DeepEqualK[A1, A2]): DeepEqual[A1[Ptr1], A2[Ptr2], Ptr1, Ptr2] =
    ev.specialize

  implicit def optionInstance[Ptr1[_], Ptr2[_], A1, A2](implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): DeepEqual[Option[A1], Option[A2], Ptr1, Ptr2] =
    new DeepEqual[Option[A1], Option[A2], Ptr1, Ptr2] {
      def equal(a1: Option[A1], a2: Option[A2]): IsEqual[Ptr1, Ptr2] = IsEqual.optionEqual(a1, a2)
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
}

trait DeepEqualK[F1[_[_]], F2[_[_]]] {
  def equal[Ptr1[_], Ptr2[_]](f1: F1[Ptr1], f2: F2[Ptr2]): IsEqual[Ptr1, Ptr2]

  def specialize[Ptr1[_], Ptr2[_]]: DeepEqual[F1[Ptr1], F2[Ptr2], Ptr1, Ptr2] =
    new DeepEqual[F1[Ptr1], F2[Ptr2], Ptr1, Ptr2] {
      def equal(f1: F1[Ptr1], f2: F2[Ptr2]): IsEqual[Ptr1, Ptr2] = DeepEqualK.this.equal(f1, f2)
    }
}

sealed trait IsEqual[Ptr1[_], Ptr2[_]] {
  import IsEqual._

  def &&(that: => IsEqual[Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = And(this, () => that)
  def ||(that: => IsEqual[Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = Or(this, () => that)
}

object IsEqual {
  private[util] case class Const[Ptr1[_], Ptr2[_]](value: Boolean) extends IsEqual[Ptr1, Ptr2]
  private[util] case class Indirect[Ptr1[_], Ptr2[_], X, Y](p1: Ptr1[X], p2: Ptr2[Y], ev: DeepEqual[X, Y, Ptr1, Ptr2]) extends IsEqual[Ptr1, Ptr2]
  private[util] case class And[Ptr1[_], Ptr2[_]](e1: IsEqual[Ptr1, Ptr2], e2: () => IsEqual[Ptr1, Ptr2]) extends IsEqual[Ptr1, Ptr2]
  private[util] case class Or[Ptr1[_], Ptr2[_]](e1: IsEqual[Ptr1, Ptr2], e2: () => IsEqual[Ptr1, Ptr2]) extends IsEqual[Ptr1, Ptr2]

  def apply[Ptr1[_], Ptr2[_]](value: Boolean): IsEqual[Ptr1, Ptr2] = Const(value)
  def apply[Ptr1[_], Ptr2[_], X, Y](p1: Ptr1[X], p2: Ptr2[Y])(implicit ev: DeepEqual[X, Y, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = Indirect(p1, p2, ev)

  def equal[Ptr1[_], Ptr2[_], A1, A2](a1: A1, a2: A2)(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] = ev.equal(a1, a2)

  def optionEqual[Ptr1[_], Ptr2[_], A1, A2](o1: Option[A1], o2: Option[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    (o1, o2) match {
      case (Some(a1), Some(a2)) => ev.equal(a1, a2)
      case (None, None) => Const(true)
      case _ => Const(false)
    }

  def listEqual[Ptr1[_], Ptr2[_], A1, A2](l1: List[A1], l2: List[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    if(l1.size != l2.size) Const(false)
    else {
      def go(l1: List[A1], l2: List[A2]): IsEqual[Ptr1, Ptr2] = (l1, l2) match {
        case (a :: as, b :: bs) => ev.equal(a, b) && go(as, bs)
        case _ => Const(true)
      }
      go(l1, l2)
    }

  def vectorEqual[Ptr1[_], Ptr2[_], A1, A2](v1: Vector[A1], v2: Vector[A2])(implicit ev: DeepEqual[A1, A2, Ptr1, Ptr2]): IsEqual[Ptr1, Ptr2] =
    if(v1.size != v2.size) Const(false)
    else {
      def go(i: Int): IsEqual[Ptr1, Ptr2] =
        if(i >= 0) ev.equal(v1(i), v2(i)) && go(i-1)
        else Const(true)

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

}