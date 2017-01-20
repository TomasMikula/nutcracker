package nutcracker.util.ops

import nutcracker.util.Aggregator

import scalaz.Foldable

final case class FoldableOps[F[_], A](fa: F[A]) extends AnyVal {

  /** Like `foldLeft`, but allows the function to be resolved implicitly. */
  def aggregateLeft[B](b: B)(implicit agg: Aggregator[B, A], F: Foldable[F]): B =
    F.foldLeft(fa, b)(agg.append)
}

trait ToFoldableOps {
  def toFoldableOps[F[_], A](fa: F[A]): FoldableOps[F, A] = FoldableOps(fa)
}