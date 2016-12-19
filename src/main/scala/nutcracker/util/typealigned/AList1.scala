package nutcracker.util.typealigned

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.{Compose, Leibniz}


/**
 * Type-aligned list with at least 1 element.
 * Example:
 *
 * {{{
 * F[A, X], F[X, Y], F[Y, Z], F[Z, B]
 * }}}
 */
sealed abstract class AList1[F[_, _], A, B] {
  import AList1._

  def ::[Z](fza: F[Z, A]): AList1[F, Z, B] =
    ACons(fza, this)

  def uncons: Either[F[A, B], APair[F[A, ?], AList1[F, ?, B]]] =
    this match {
      case AJust(f) => Left(f)
      case ACons(h, t) => Right(APair.of[F[A, ?], AList1[F, ?, B]](h, t))
    }

  def :::[Z](that: AList1[F, Z, A]): AList1[F, Z, B] = {
    @inline def pair[X](rev: Composed1[F, Z, X], fs: AList1[F, X, B]) =
      APair[Composed1[F, Z, ?], AList1[F, ?, B], X](rev, fs)

    @tailrec def go(p: APair[Composed1[F, Z, ?], AList1[F, ?, B]]): AList1[F, Z, B] = {
      val (rev, fs) = (p._1, p._2)
      rev.uncons match {
        case Left(f) => f :: fs
        case Right(ht) => go(pair(ht._2, ht._1 :: fs))
      }
    }
    go(pair(that.reverse, this))
  }

  def :::[Z](that: AList[F, Z, A]): AList1[F, Z, B] =
    that match {
      case ANil(ev) => Leibniz.symm[Nothing, Any, Z, A](ev).subst[AList1[F, ?, B]](this)
      case ASome(list) => list ::: this
    }

  def ++[C](that: AList1[F, B, C]): AList1[F, A, C] =
    this ::: that

  def reverse: Composed1[F, A, B] = {
    @inline def pair[X](acc: Composed1[F, A, X], fs: AList1[F, X, B]) =
      APair[Composed1[F, A, ?], AList1[F, ?, B], X](acc, fs)

    @tailrec def go(p: APair[Composed1[F, A, ?], AList1[F, ?, B]]): Composed1[F, A, B] = {
      val (acc, fs) = (p._1, p._2)
      fs match {
        case AJust(f) => f :: acc
        case ACons(h, t) => go(pair(h :: acc, t))
      }
    }

    this match {
      case AJust(f) => AList1.op[F, A, B](f)
      case ACons(h, t) => go(pair(AList1.op(h), t))
    }
  }

  def foldLeft[G[_]](ga: G[A])(implicit G: FunctorLike[G, F]): G[B] = {
    @inline def pair[X](gx: G[X], fb: AList1[F, X, B]) =
      APair[G, AList1[F, ?, B], X](gx, fb)

    @tailrec def go(p: APair[G, AList1[F, ?, B]]): G[B] = {
      val (gx, fs) = (p._1, p._2)
      fs match {
        case AJust(f) => G.map(gx)(f)
        case ACons(f, g) => go(pair(G.map(gx)(f), g))
      }
    }

    go(pair(ga, this))
  }

  def foldRight[G[_]](gb: G[B])(implicit G: ContravariantLike[G, F]): G[A] =
    reverse.foldLeft(gb)(G)

  def reduceLeft(implicit F: Compose[F]): F[A, B] =
    this match {
      case AJust(f) => f
      case ACons(h, t) => t.foldLeft[F[A, ?]](h)
    }

  /**
   * Compose the elements of this list in a balanced binary fashion.
   */
  def reduce(implicit F: Compose[F]): F[A, B] =
    this match {
      case ACons(f, g) => g.foldLeft[LBinary[F, A, ?]](LBinary(f)).extractL
      case AJust(f) => f
    }
}

final case class AJust[F[_, _], A, B](value: F[A, B]) extends AList1[F, A, B]
final case class ACons[F[_, _], A, B, C](head: F[A, B], tail: AList1[F, B, C]) extends AList1[F, A, C]

object AList1 {
  /**
   * Reversed type-aligned list is type-aligned with flipped type constructor.
   * For example, when we reverse
   *
   * {{{
   * A => B, B => C, C => D, D => E
   * }}}
   *
   * we get
   *
   * {{{
   * D => E, C => D, B => C, A => B
   * }}}
   *
   * which is type-aligned if we flip the arrows:
   *
   * {{{
   * E <= D, D <= C, C <= B, B <= A
   * }}}
   *
   * The first list has type `AList1[=>, A, E]`, while
   * the reversed list has type `Composed1[=>, A, E]`.
   */
  type Composed1[F[_, _], A, B] = AList1[Op[F, ?, ?], B, A]

  def apply[F[_, _], A, B](f: F[A, B]): AList1[F, A, B] = AJust(f)
  def op[F[_, _], A, B](f: F[A, B]): Composed1[F, A, B] = apply[Op[F, ?, ?], B, A](f)
}
