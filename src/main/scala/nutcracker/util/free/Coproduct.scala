package nutcracker.util.free

import scalaz.~>

object Coproduct {
  def injectLeft[F[_], G[_], H[_]](f: F ~> G): ({ type Out[A] = F[A] Either H[A] })#Out ~> ({ type Out[A] = G[A] Either H[A] })#Out =
    new (({ type Out[A] = F[A] Either H[A] })#Out ~> ({ type Out[A] = G[A] Either H[A] })#Out) {
      override def apply[A](ca: F[A] Either H[A]): (G[A] Either H[A]) =
        ca match {
          case Left(fa) => Left(f(fa))
          case Right(ha) => Right(ha)
        }
    }

  def injectRight[F[_], G[_], H[_]](f: G ~> H): ({ type Out[A] = F[A] Either G[A] })#Out ~> ({ type Out[A] = F[A] Either H[A] })#Out =
    new (({ type Out[A] = F[A] Either G[A] })#Out ~> ({ type Out[A] = F[A] Either H[A] })#Out) {
      override def apply[A](ca: F[A] Either G[A]): (F[A] Either H[A]) =
        ca match {
          case Left(fa) => Left(fa)
          case Right(ga) => Right(f(ga))
        }
    }
}