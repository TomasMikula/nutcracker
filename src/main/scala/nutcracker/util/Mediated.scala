package nutcracker.util

import scalaz.{Bind, Functor}
import scalaz.syntax.bind._

/** Computation of `C` with a missing link between `A` and `B`.
  * Encapsulates effectful computations `() ~~~> A` and `B ~~~> C`.
  * By plugging in `A ~~~> B` one can obtain `() ~~~> C`.
  * The purpose of this class is to prevent using just one of the two
  * encapsulated computations: either both, or none. This is useful when
  * the effects of the two computations are somehow paired and this pairing
  * can't be broken. This is only safe when `M` is a lazy monad with benign
  * effects.
  */
final class Mediated[M[_], A, B, C](private val value: M[(A, B => M[C])]) extends AnyVal {

  def complete(f: A => B)(implicit M: Bind[M]): M[C] = for {
    ag <- value
    (a, g) = ag
    c <- g(f(a))
  } yield c

  def completeM(f: A => M[B])(implicit M: Bind[M]): M[C] = for {
    ag <- value
    (a, g) = ag
    b <- f(a)
    c <- g(b)
  } yield c

  def flatMap[D, E](f: A => Mediated[M, B, D, E])(implicit M: Bind[M]): Mediated[M, C, D, E] = Mediated(for {
    ag <- value
    (a, g) = ag
    bh <- f(a).value
    (b, h) = bh
    c <- g(b)
  } yield (c, h))

  def fst[S, T](f: S => T)(implicit M: Functor[M]): Mediated[M, A, (S, B), (T, C)] =
    Mediated(M.map(value) { case (a, g) => (a, sb => M.map(g(sb._2))(c => (f(sb._1), c))) })

  def snd[S, T](f: S => T)(implicit M: Functor[M]): Mediated[M, A, (B, S), (C, T)] =
    Mediated(M.map(value) { case (a, g) => (a, bs => M.map(g(bs._1))(c => (c, f(bs._2)))) })
}

object Mediated {
  def apply[M[_], A, B, C](value: M[(A, B => M[C])]): Mediated[M, A, B, C] =
    new Mediated[M, A, B, C](value)
}