package nutcracker.util.ops

import nutcracker.util.Aggregator
import scala.language.implicitConversions
import scalaz.Foldable

object foldable extends ToFoldableOps

trait ToFoldableOps {
  implicit def toFoldableOps[F[_], A](fa: F[A]): FoldableOps[F, A] = FoldableOps(fa)
}

final case class FoldableOps[F[_], A](fa: F[A]) extends AnyVal {

  /** Like `foldLeft`, but allows the function to be resolved implicitly. */
  def aggregateLeft[B](b: B)(implicit agg: Aggregator[B, A], F: Foldable[F]): B =
    F.foldLeft(fa, b)(agg.append)
}