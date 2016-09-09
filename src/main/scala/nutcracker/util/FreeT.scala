package nutcracker.util

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.{Applicative, BindRec, \/, -\/, \/-, ~>}

sealed abstract class FreeT[S[_], M[_], A] {
  import FreeT._

  def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    FlatMap(this, f)

  def map[B](f: A => B)(implicit M: Applicative[M]): FreeT[S, M, B] =
    flatMap(a => pure(f(a)))

  def foldMap(f: S ~> M)(implicit M0: BindRec[M], M1: Applicative[M]): M[A] =
    M0.bind(resume0) {
      case \/-(a) => M1.point(a)
      case -\/(fa) => fa.foldMap(f)
    }

  def hoist[N[_]](f: M ~> N): FreeT[S, N, A] = step match {
    case Pure(ma) => Pure(f(ma))
    case Suspend(sa) => Suspend(sa)
    case FlatMap(fz, g) => fz.hoist(f).flatMap(g(_).hoist(f))
  }

  def interpret[T[_]](f: S ~> T): FreeT[T, M, A] = step match {
    case Pure(ma) => Pure(ma)
    case Suspend(sa) => Suspend(f(sa))
    case FlatMap(fz, g) => fz.interpret(f).flatMap(g(_).interpret(f))
  }

  @tailrec
  private def step: FreeT[S, M, A] = this match {
    case FlatMap(FlatMap(fz, f), g) => fz.flatMap((f(_).flatMap(g))).step
    case _ => this
  }

  private def resume0(implicit M0: BindRec[M], M1: Applicative[M]): M[FreeT1[S, M, A] \/ A] = {
    @tailrec
    def go(fa: FreeT[S, M, A]): M[FreeT[S, M, A] \/ (FreeT1[S, M, A] \/ A)] = fa match {
      case Pure(ma) => M0.map(ma)(a => \/-(\/-(a)))
      case Suspend(sa) => M1.point(\/-(-\/(Suspend1(sa))))
      case FlatMap(fz, g) => fz match {
        case Pure(mz) => M0.map(mz)(z => -\/(g(z)))
        case Suspend(sz) => M1.point(\/-(-\/(FlatMap1(sz, g))))
        case FlatMap(fy, f) => go(fy.flatMap(f(_).flatMap(g)))
      }
    }

    M0.tailrecM(this)(go)
  }
}

object FreeT {
  private final case class Pure[S[_], M[_], A](ma: M[A]) extends FreeT[S, M, A]
  private final case class Suspend[S[_], M[_], A](sa: S[A]) extends FreeT[S, M, A]
  private final case class FlatMap[S[_], M[_], Z0, A](z0: FreeT[S, M, Z0], f0: Z0 => FreeT[S, M, A]) extends FreeT[S, M, A]

  /** FreeT that contains at least 1 suspension, at the very top. */
  private sealed abstract class FreeT1[S[_], M[_], A] {
    def toFreeT: FreeT[S, M, A] = this match {
      case Suspend1(sa) => Suspend(sa)
      case FlatMap1(sz, f) => Suspend(sz).flatMap(f)
    }

    def foldMap(f: S ~> M)(implicit M: BindRec[M], M1: Applicative[M]): M[A] =
      M.tailrecM(this)(_ match {
        case Suspend1(sa) => M.map(f(sa))(\/.right)
        case FlatMap1(sz, g) => M.bind(f(sz))(z => g(z).resume0)
      })
  }
  private final case class Suspend1[S[_], M[_], A](sa: S[A]) extends FreeT1[S, M, A]
  private final case class FlatMap1[S[_], M[_], Z, A](sz: S[Z], f: Z => FreeT[S, M, A]) extends FreeT1[S, M, A]

  def pure[S[_], M[_], A](a: A)(implicit M: Applicative[M]): FreeT[S, M, A] =
    Pure(M.point(a))

  def point[S[_], M[_], A](a: A)(implicit M: Applicative[M]): FreeT[S, M, A] =
    pure(a)

  def liftF[S[_], M[_], A](sa: S[A]): FreeT[S, M, A] =
    Suspend(sa)

}
