package nutcracker.util

import scala.language.higherKinds
import scalaz.Equal

trait EqualK[F[_]] {
  def equal[A](f1: F[A], f2: F[A]): Boolean

  def specialize[A]: Equal[F[A]] = new Equal[F[A]] {
    def equal(a1: F[A], a2: F[A]): Boolean = EqualK.this.equal(a1, a2)
  }
}

object EqualK {
  implicit def specialize[F[_], A](implicit ev: EqualK[F]): Equal[F[A]] = ev.specialize[A]
}

/** Equality for type constructors of kind `* -> *` applied to possibly heterogenous types. */
trait HEqualK[F[_]] extends EqualK[F] {
  def hEqual[A, B](fa: F[A], fb: F[B]): Boolean

  override def equal[A](f1: F[A], f2: F[A]): Boolean =
    hEqual(f1, f2)
}