package nutcracker.util.free

import scalaz.Id._
import scalaz._

final case class Trampoline[A] private(unwrap: FreeBind[Id, A]) extends AnyVal {
  def apply(): A = eval
  def eval: A = unwrap.foldMap(λ[Id ~> Id](x => x))
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = Trampoline(unwrap.flatMap(a => f(a).unwrap))
}

object Trampoline {

  def done[A](a: A): Trampoline[A] =
    Trampoline(FreeBind.liftF[Id, A](a))

  def delay[A](a: => A): Trampoline[A] =
    done(()).flatMap(_ => done(a))

  def suspend[A](ta: => Trampoline[A]): Trampoline[A] =
    done(()).flatMap(_ => ta)

}