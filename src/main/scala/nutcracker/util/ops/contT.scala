package nutcracker.util.ops

import scala.language.implicitConversions
import scalaz.{Bind, ContT, IndexedContT}

object contT extends ToContTOps

trait ToContTOps {
  implicit def toContTOps[M[_], A](ma: M[A]): ContTOps[M, A] = ContTOps(ma)
}

final case class ContTOps[M[_], A](ma: M[A]) extends AnyVal {
  def cps[R](implicit M: Bind[M]): ContT[R, M, A] = ContT(M.bind(ma)(_))
  def cps_(implicit M: Bind[M]): ContT[Unit, M, A] = cps[Unit]
}

object indexedContT extends ToIndexedContTOps

trait ToIndexedContTOps {
  implicit def toIndexedContTOps[R, O, F[_], A](cps: IndexedContT[R, O, F, A]): IndexedContTOps[R, O, F, A] =
    IndexedContTOps(cps)
}

final case class IndexedContTOps[R, O, F[_], A](self: IndexedContT[R, O, F, A]) extends AnyVal {
  def absorbEffect[B](implicit ev: A =:= F[B], F: Bind[F]): IndexedContT[R, O, F, B] =
    self.flatMap(a => IndexedContT.liftM(ev(a)))
}