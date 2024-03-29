package nutcracker.util.typealigned

import scalaz.{Leibniz, ===}

/** Similar to `Option[F[A, B]]`, but the empty case witnesses type equality
  * between `A` and `B`.
  */
sealed abstract class AOption[F[_, _], A, B] {
  def fold[R](
    caseSome: F[A, B] => R,
    caseNone: A === B => R,
  ): R =
    this match {
      case ASome(f)  => caseSome(f)
      case ANone(ev) => caseNone(ev)
    }
}

/**
  * The empty case contains evidence of type equality
  * to overcome the limitations of pattern-matching on GADTs.
  */
case class ANone[F[_, _], A, B](ev: A === B) extends AOption[F, A, B]
case class ASome[F[_, _], A, B](value: F[A, B]) extends AOption[F, A, B]

object AOption {
  def empty[F[_, _], A]: AOption[F, A, A] = ANone(Leibniz.refl[A])
}