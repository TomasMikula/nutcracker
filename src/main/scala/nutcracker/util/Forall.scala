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
  private lazy val value: F[Nothing, Nothing] = compute[Nothing, Nothing]

  protected def compute[K[_], A]: F[K, A]

  final def apply[K[_], A]: F[K, A] = value.asInstanceOf[F[K, A]]

  type PApplied[K[_]] = `Forall{* -> *}`[F[K, *]]

  final def papply[K[_]]: PApplied[K] =
    new PApplied[K] {
      override def compute[A]: F[K, A] = self.compute[K, A]
    }

  final def curried: `Forall{(* -> *) -> *}`[PApplied] =
    new `Forall{(* -> *) -> *}`[PApplied] {
      override def compute[K[_]]: PApplied[K] = papply[K]
    }

  /** Transform this value by a universally quantified function
    * `f: ∀ K[_], A. F[K, A] => G[K, A]`
    */
  final def transform[G[_[_], _]](f: F ≈~> G): `Forall{(* -> *) -> * -> *}`[G] = new `Forall{(* -> *) -> * -> *}`[G] {
    def compute[K[_], A]: G[K, A] = f(self.compute[K, A])
  }
}