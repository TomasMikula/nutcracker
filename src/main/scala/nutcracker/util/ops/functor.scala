package nutcracker.util.ops

import scala.language.implicitConversions
import scalaz.Functor
import scalaz.syntax.functor._

object functor extends ToFunctorOps

trait ToFunctorOps {
  implicit def toFunctorWildcardOps[F[_]](f_ : F[_]): FunctorWildcardOps[F] = FunctorWildcardOps(f_)
}

final case class FunctorWildcardOps[F[_]](f_ : F[_]) extends AnyVal {

  def void(implicit F: Functor[F]): F[Unit] =
    FunctorWildcardOps.void(f_)

}

object FunctorWildcardOps {
  def void[F[_]](f_ : F[_])(implicit F: Functor[F]): F[Unit] = {
    // unsafe cast needed because of https://github.com/lampepfl/dotty/issues/14008
    f_.asInstanceOf[F[Any]].map(_ => ())
  }
}