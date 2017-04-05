package nutcracker.util.ops

import scala.language.implicitConversions
import scalaz.{Bind, ContT, IndexedContT}

object contT extends ToContTOps

trait ToContTOps {
  implicit def toContTOps[M[_], A](ma: M[A]): ContTOps[M, A] = ContTOps(ma)
}

final case class ContTOps[M[_], A](ma: M[A]) extends AnyVal {
  def cps[R](implicit M: Bind[M]): ContT[M, R, A] = ContT(M.bind(ma)(_))
  def cps_(implicit M: Bind[M]): ContT[M, Unit, A] = cps[Unit]
}

object indexedContT extends ToIndexedContTOps

trait ToIndexedContTOps {
  implicit def toIndexedContTOps[F[_], R, O, A](cps: IndexedContT[F, R, O, A]): IndexedContTOps[F, R, O, A] =
    IndexedContTOps(cps)
}

final case class IndexedContTOps[F[_], R, O, A](self: IndexedContT[F, R, O, A]) extends AnyVal {
  def absorbEffect[B](implicit ev: A =:= F[B], F: Bind[F]): IndexedContT[F, R, O, B] =
    self.flatMap(a => IndexedContT.liftM(ev(a)))
}