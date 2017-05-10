package nutcracker.util

import scalaz.{Semigroup, ~>}

/** Represents the kind of function that one passes to `foldLeft`, as a typeclass. */
trait Aggregator[B, A] extends ((B, A) => B)

object Aggregator {
  implicit def semigroupAggregator[A](implicit A: Semigroup[A]): Aggregator[A, A] =
    A.append(_, _)

  implicit val stringBuilderAggregator: Aggregator[StringBuilder, String] =
    _ append _
}

abstract class AggregatorK[B, F[_]] {
  def apply[A](b: B, fa: F[A]): B

  def append[A](b: B, fa: F[A]): B = apply(b, fa)
}

object AggregatorK {
  def by[B, F[_], G[_]](fg: F ~> G)(implicit bg: AggregatorK[B, G]): AggregatorK[B, F] =
    new AggregatorK[B, F] {
      def apply[A](b: B, fa: F[A]): B = bg(b, fg(fa))
    }

  def byConst[B, F[_], A](fa: F ~> λ[α => A])(implicit ba: Aggregator[B, A]): AggregatorK[B, F] =
    new AggregatorK[B, F] {
      def apply[X](b: B, fx: F[X]): B = ba(b, fa(fx))
    }
}