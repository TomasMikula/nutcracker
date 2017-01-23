package nutcracker.util.typealigned

import scala.language.higherKinds
import scalaz.{Leibniz, \/, \/-, ~>}
import Leibniz.===

/**
 * A potentially empty type-aligned list.
 */
sealed abstract class AList[F[_, _], A, B] {
  import AList._

  def ::[Z](fza: F[Z, A]): AList1[F, Z, B] =
    this match {
      case ANil(ev) => ev.subst[AList1[F, Z, ?]](AList1(fza))
      case ASome(list) => fza :: list
    }

  def +:[Z](fza: F[Z, A]): AList[F, Z, B] = (fza :: this).toList

  def :::[Z](that: AList[F, Z, A]): AList[F, Z, B] =
    this match {
      case ANil(ev) => ev.subst[AList[F, Z, ?]](that)
      case ASome(list) => ASome(that ::: list)
    }

  def reverse: Composed[F, A, B] =
    this match {
      case ANil(ev) => ANil[Op[F, ?, ?], B, A](Leibniz.symm[Nothing,Any,A,B](ev))
      case ASome(list) => ASome[Op[F, ?, ?], B, A](list.reverse)
    }

  def foldLeft[G[_]](ga: G[A])(implicit G: FunctorLike[G, F]): G[B] =
    this match {
      case ANil(ev) => ev.subst(ga)
      case ASome(list) => list.foldLeft(ga)
    }

  def foldLeftWhile[G[_], H[_]](ga: G[A])(tr: λ[α => APair[G, F[?, α]]] ~> λ[α => H[α] \/ G[α]]): APair[H, AList[F, ?, B]] \/ G[B] =
    this match {
      case ANil(ev) => \/-(ev.subst(ga))
      case ASome(list) => list.foldLeftWhile(ga)(tr)
    }

  def foldRight[G[_]](gb: G[B])(implicit G: ContravariantLike[G, F]): G[A] =
    reverse.foldLeft(gb)(G)
}

/**
 * The empty case contains the evidence of type equality,
 * to overcome the limitations of pattern-matching on GADTs.
 */
final case class ANil[F[_, _], A, B](ev: A === B) extends AList[F, A, B]
final case class ASome[F[_, _], A, B](value: AList1[F, A, B]) extends AList[F, A, B]

object AList {

  type Op[F[_, _], A, B] = F[B, A]
  type Composed[F[_, _], A, B] = AList[Op[F, ?, ?], B, A]

  def empty[F[_, _], A]: AList[F, A, A] = ANil(Leibniz.refl[A])
}
