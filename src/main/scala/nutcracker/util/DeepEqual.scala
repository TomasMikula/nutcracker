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
}