package nutcracker.util

import scala.language.higherKinds

trait ValKA[F[_[_], _]] {
  import nutcracker.util.ValKA._

  private lazy val value: F[Nothing, Nothing] = compute[Nothing, Nothing]

  protected def compute[K[_], A]: F[K, A]

  final def apply[K[_], A]: F[K, A] = value.asInstanceOf[F[K, A]]
  final def papply[K[_]]: ValA[F[K, ?]] = PAppliedValKA(this)
  final def curried: ValK[λ[K[_] => ValA[F[K, ?]]]] = CurriedValKA(this)
}

object ValKA {
  private case class CurriedValKA[F[_[_], _]](run: ValKA[F]) extends ValK[λ[K[_] => ValA[F[K, ?]]]] {
    protected final def compute[K[_]]: ValA[F[K, ?]] = run.papply[K]
  }
  private case class PAppliedValKA[F[_[_], _], K[_]](run: ValKA[F]) extends ValA[F[K, ?]] {
    protected final def compute[A]: F[K, A] = run[K, A]
  }
}