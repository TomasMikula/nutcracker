package nutcracker

import scala.language.higherKinds

import nutcracker.util.ContU

trait Defer[M[_], D] {
  def defer(delay: D, k: M[Unit]): M[Unit]

  /** Defer registration of callbacks to the given CPS computation. */
  def deferC[A](delay: D, c: ContU[M, A]): ContU[M, A] =
    ContU(f => defer(delay, c(f)))
}
