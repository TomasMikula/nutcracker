package nutcracker.util

import scalaz.~>

sealed abstract class Exists[F[_]] {
  type X
  val value: F[X]

  def map[G[_]](f: F ~> G): Exists[G] =
    Exists(f(value))

  override def toString: String =
    s"∃($value)"

  override def equals(that: Any): Boolean =
    that match {
      case that: Exists[_] => this.value == that.value
      case _               => false
    }

  override def hashCode: Int =
    value.hashCode
}

object Exists {
  def apply[F[_], A](fa: F[A]): Exists[F] = new Exists[F] {
    type X = A
    val value = fa
  }
}

sealed abstract class `Exists{(* -> *) -> *}`[F[_[_]]] {
  type X[_]
  val value: F[X]

  def map[G[_[_]]](f: F ≈> G): `Exists{(* -> *) -> *}`[G] =
    `Exists{(* -> *) -> *}`(f(value))
}

object `Exists{(* -> *) -> *}` {

  def apply[F[_[_]], A[_]](fa: F[A]): `Exists{(* -> *) -> *}`[F] =
    new `Exists{(* -> *) -> *}`[F] {
      type X[a] = A[a]
      val value = fa
    }

  def apply[F[_[_]]]: SyntaxHelper[F] = SyntaxHelper[F]()

  final case class SyntaxHelper[F[_[_]]]() {
    def from[A[_]](fa: F[A]): `Exists{(* -> *) -> *}`[F] = apply[F, A](fa)
  }
}