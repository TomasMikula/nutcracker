package nutcracker.util

import scalaz.~>

/** Universally quantified value:
  * `∀ A. F[A]`
  */
trait `Forall{* -> *}`[F[_]] { self =>
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[A]: F[A]

  final def apply[A]: F[A] = value.asInstanceOf[F[A]]

  /** Transform this value by a universally quantified function
    * `f: ∀ A. F[A] => G[A]`
    */
  final def transform[G[_]](f: F ~> G): `Forall{* -> *}`[G] = new `Forall{* -> *}`[G] {
    def compute[A]: G[A] = f(self.compute[A])
  }
}


/** Universally quantified value:
  * `∀ K[_]. F[K]`
  */
trait `Forall{(* -> *) -> *}`[F[_[_]]] { self =>
  private lazy val value: F[Nothing] = compute[Nothing]

  protected def compute[K[_]]: F[K]

  final def apply[K[_]]: F[K] = value.asInstanceOf[F[K]]

  /** Transform this value by a universally quantified function
    * `f: ∀ K[_]. F[K] => G[K]`
    */
  final def transform[G[_[_]]](f: F ≈> G): `Forall{(* -> *) -> *}`[G] = new `Forall{(* -> *) -> *}`[G] {
    def compute[K[_]]: G[K] = f(self.compute[K])
  }
}


/** Universally quantified value:
  * `∀ K[_], A. F[K, A]`
  */
trait `Forall{(* -> *) -> * -> *}`[F[_[_], _]] { self =>
  import nutcracker.util.`Forall{(* -> *) -> * -> *}`._

  private lazy val value: F[Nothing, Nothing] = compute[Nothing, Nothing]

  protected def compute[K[_], A]: F[K, A]

  final def apply[K[_], A]: F[K, A] = value.asInstanceOf[F[K, A]]
  final def papply[K[_]]: `Forall{* -> *}`[F[K, *]] = PApplied(this)
  final def curried: `Forall{(* -> *) -> *}`[λ[K[_] => `Forall{* -> *}`[F[K, *]]]] = Curried(this)

  /** Transform this value by a universally quantified function
    * `f: ∀ K[_], A. F[K, A] => G[K, A]`
    */
  final def transform[G[_[_], _]](f: F ≈~> G): `Forall{(* -> *) -> * -> *}`[G] = new `Forall{(* -> *) -> * -> *}`[G] {
    def compute[K[_], A]: G[K, A] = f(self.compute[K, A])
  }
}

object `Forall{(* -> *) -> * -> *}` {
  private case class Curried[F[_[_], _]](run: `Forall{(* -> *) -> * -> *}`[F]) extends `Forall{(* -> *) -> *}`[λ[K[_] => `Forall{* -> *}`[F[K, *]]]] {
    protected final def compute[K[_]]: `Forall{* -> *}`[F[K, *]] = run.papply[K]
  }
  private case class PApplied[F[_[_], _], K[_]](run: `Forall{(* -> *) -> * -> *}`[F]) extends `Forall{* -> *}`[F[K, *]] {
    protected final def compute[A]: F[K, A] = run[K, A]
  }
}