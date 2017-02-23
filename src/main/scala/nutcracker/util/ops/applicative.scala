package nutcracker.util.ops

import scala.language.implicitConversions
import scalaz.{Applicative}
import scalaz.std.vector._
import scalaz.syntax.traverse._

object applicative extends ToApplicativeOps

trait ToApplicativeOps {
  implicit def toApplicativeOps[F[_], A](fa: F[A]): ApplicativeOps[F, A] = ApplicativeOps(fa)
}

final case class ApplicativeOps[F[_], A](fa: F[A]) extends AnyVal {

  def replicate(n: Int)(implicit F: Applicative[F]): F[Vector[A]] =
    Vector.fill(n)(fa).sequence

}