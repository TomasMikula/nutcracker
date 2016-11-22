package nutcracker.util

import scala.language.higherKinds
import scalaz.Equal
import scalaz.Id.Id

trait EqualK[F[_]] {
  def equal[A](f1: F[A], f2: F[A]): Boolean

  def specialize[A]: Equal[F[A]] = new Equal[F[A]] {
    def equal(a1: F[A], a2: F[A]): Boolean = EqualK.this.equal(a1, a2)
  }
}

object EqualK {
  implicit def specialize[F[_], A](implicit ev: EqualK[F]): Equal[F[A]] = ev.specialize[A]

  implicit def homogenize[F[_]](implicit ev: HEqualK[F]): EqualK[F] = ev.homogenize
}

/** Equality for type constructors of kind `* -> *` applied to possibly heterogenous types. */
trait HEqualK[F[_]] {
  def hEqual[A, B](fa: F[A], fb: F[B]): Boolean

  def homogenize: EqualK[F] = new EqualK[F] {
    override def equal[A](f1: F[A], f2: F[A]): Boolean =
      hEqual(f1, f2)
  }
}

object HEqualK {

  /** Reference comparison. For value types might return `false` even for equal
    * values, since they must be boxed before reference comparison can take place.
    */
  implicit val referenceEquality: HEqualK[Id] = new HEqualK[Id] {
    def hEqual[A, B](fa: Id[A], fb: Id[B]): Boolean = {
      val (a: A) = fa
      val (b: B) = fb
      a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]
    }
  }
}