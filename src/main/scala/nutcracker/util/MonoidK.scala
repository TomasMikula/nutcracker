package nutcracker.util

import scala.language.higherKinds
import scalaz.Monoid

trait MonoidK[F[_]] { self =>

  def zero[A]: F[A]
  def append[A](f1: F[A], f2: F[A]): F[A]

  def monoid[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def zero: F[A] = self.zero
    def append(f1: F[A], f2: => F[A]): F[A] = self.append(f1, f2)
  }
}
