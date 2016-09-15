package nutcracker.util

import scalaz.{-\/, \/, \/-}

trait Inject[A, B] {
  def apply(a: A): B = inj(a)
  def inj(a: A): B
}

object Inject {
  implicit def idInject[A]: Inject[A, A] = new Inject[A, A] {
    def inj(a: A): A = a
  }

  implicit def leftInject[A, B, C](implicit i: Inject[A, B]): Inject[A, B \/ C] = new Inject[A, B \/ C] {
    def inj(a: A): B \/ C = -\/(i(a))
  }

  implicit def rightInject[A, B, C](implicit i: Inject[A, C]): Inject[A, B \/ C] = new Inject[A, B \/ C] {
    def inj(a: A): B \/ C = \/-(i(a))
  }
}