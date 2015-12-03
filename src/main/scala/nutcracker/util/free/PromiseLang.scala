package nutcracker.util.free

import scala.language.higherKinds

/**
  *
  * @tparam K type of continuation that can be registered to execute
  *           when promise is completed
  * @tparam A type of promised value
  */
sealed trait PromiseLang[K[_], A]

object PromiseLang {

  final case class Promised[A] private (private val id: Long)

  case class Promise[K[_], A]() extends PromiseLang[K, Promised[A]]
  case class Complete[K[_], A](p: Promised[A], a: A) extends PromiseLang[K, Unit]
  case class OnComplete[K[_], A](p: Promised[A], f: A => K[Unit]) extends PromiseLang[K, Unit]
}