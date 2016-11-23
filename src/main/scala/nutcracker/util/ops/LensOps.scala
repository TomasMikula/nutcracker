package nutcracker.util.ops

import scala.language.implicitConversions

import scalaz.{Lens, Store}

final case class LensOps[S](s: S) extends AnyVal {
  def focus[A](lens: Lens[S, A]): Store[A, S] = lens(s)
}

trait ToLensOps {
  implicit def toLensOps[S](s: S): LensOps[S] = LensOps(s)
}