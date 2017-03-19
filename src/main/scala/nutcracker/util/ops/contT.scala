package nutcracker.util.ops

import scala.language.implicitConversions
import scalaz.{Bind, ContT}

object contT extends ToContTOps

trait ToContTOps {
  implicit def toContTOps[M[_], A](ma: M[A]): ContTOps[M, A] = ContTOps(ma)
}

final case class ContTOps[M[_], A](ma: M[A]) extends AnyVal {
  def cps[R](implicit M: Bind[M]): ContT[M, R, A] = ContT(M.bind(ma)(_))
  def cps_(implicit M: Bind[M]): ContT[M, Unit, A] = cps[Unit]
}