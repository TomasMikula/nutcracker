package nutcracker.util

import nutcracker.util.typealigned.BalancedAppender

import scalaz.{Semigroup, ~>}

/** Represents the kind of function that one passes to `foldLeft`. */
final case class Aggregator[B, A](append: (B, A) => B) extends AnyVal {
  def apply(b: B, a: A): B = append(b, a)
}

object Aggregator {
  implicit def semigroupAggregator[A](implicit A: Semigroup[A]): Aggregator[A, A] =
    Aggregator((a0, a1) => A.append(a0, a1))

  implicit val stringBuilderAggregator: Aggregator[StringBuilder, String] =
    Aggregator(_ append _)

  implicit def appenderAggregator[A](implicit A: Semigroup[A]): Aggregator[BalancedAppender[A], A] =
    Aggregator((acc, a) => acc.append(a))
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