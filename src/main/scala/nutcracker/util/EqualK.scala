package nutcracker.util

import scalaz.Equal
import scalaz.Id.Id

/** Universally quantified equality, isomorphic to `∀A. Equal[F[A]]`.
  *
  * Note that equality on `A` is not required to compare values of `F[A]`.
  */
trait EqualK[F[_]] {
  def equalK[A](f1: F[A], f2: F[A]): Boolean

  def specialize[A]: Equal[F[A]] =
    (a1, a2) => equalK(a1, a2)
}

object EqualK {
  implicit def specialize[F[_], A](implicit ev: EqualK[F]): Equal[F[A]] = ev.specialize[A]
}

/** Heterogeneous equality, i.e. able to compare values of two disparate types. */
trait HEqual[A, B] { self =>
  def hEqual(a: A, b: B): Boolean

  def flip: HEqual[B, A] = new HEqual[B, A] {
    override def hEqual(b: B, a: A): Boolean = self.hEqual(a, b)
    override def flip: HEqual[A, B] = self
  }
}

object HEqual {
  implicit def specialized[F[_], A, B](implicit ev: HEqualK[F]): HEqual[F[A], F[B]] = ev.hSpecialize[A, B]
}

/** Universally quantified heterogeneous equality, isomorphic to `∀A,B. HEqual[F[A], F[B]]`.
  *
  * Note that (heterogeneous) equality between `A` and `B` is not required to compare `F[A]` to `F[B]`.
  */
trait HEqualK[F[_]] extends EqualK[F] {
  def hEqualK[A, B](fa: F[A], fb: F[B]): Boolean

  override def equalK[A](f1: F[A], f2: F[A]): Boolean = hEqualK(f1, f2)

  def hSpecialize[A, B]: HEqual[F[A], F[B]] =
    (fa, fb) => hEqualK(fa, fb)
}

object HEqualK {

  /** Reference comparison. For value types might return `false` even for equal
    * values, since they must be boxed before reference comparison can take place.
    */
  implicit val referenceEquality: HEqualK[Id] = new HEqualK[Id] {
    def hEqualK[A, B](fa: Id[A], fb: Id[B]): Boolean = {
      val (a: A) = fa
      val (b: B) = fb
      a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
    }
  }
}