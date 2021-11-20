package nutcracker.util

import scala.language.higherKinds

import scalaz.~>

/**
  * Universally quantified function:
  * `∀ K[_]. F[K] => G[K]`
  */
trait `FunctionK{(* -> *) -> *}`[F[_[_]], G[_[_]]] {

  def apply[K[_]](f: F[K]): G[K]
}


/**
  * Universally quantified function:
  * `∀ K[_], A. F[K, A] => G[K, A]`
  */
trait `FunctionK{(* -> *) -> * -> *}`[F[_[_], _], G[_[_], _]] {
  import nutcracker.util.`FunctionK{(* -> *) -> * -> *}`._

  def apply[K[_], A](f: F[K, A]): G[K, A]
  def papply[K[_]]: F[K, *] ~> G[K, *] = PApplied(this)
}

object `FunctionK{(* -> *) -> * -> *}` {
  private case class PApplied[F[_[_], _], G[_[_], _], K[_]](run: F ≈~> G) extends (F[K, *] ~> G[K, *]) {
    def apply[A](f: F[K, A]): G[K, A] = run(f)
  }

  implicit def idTransform[F[_[_], _]]: F ≈~> F = new (F ≈~> F) {
    def apply[K[_], A](f: F[K, A]): F[K, A] = f
  }
}