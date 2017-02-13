package nutcracker.util.typealigned

import scala.language.higherKinds
import scalaz.{\/, \/-, ~>}

/**
 * A potentially empty type-aligned list.
 */
final class AList[F[_, _], A, B] private (val uncons: AOption[AList1[F, ?, ?], A, B]) extends AnyVal {
  import AList._

  def ::[Z](fza: F[Z, A]): AList1[F, Z, B] =
    uncons match {
      case ANone(ev) => ev.subst[AList1[F, Z, ?]](AList1(fza))
      case ASome(list) => fza :: list
    }

  def +:[Z](fza: F[Z, A]): AList[F, Z, B] = (fza :: this).toList

  def :::[Z](that: AList[F, Z, A]): AList[F, Z, B] =
    uncons match {
      case ANone(ev) => ev.subst[AList[F, Z, ?]](that)
      case ASome(list) => AList(that ::: list)
    }

  def reverse: Composed[F, A, B] =
    uncons match {
      case ANone(ev) => ev.subst[AList[Op[F, ?, ?], ?, A]](empty[Op[F, ?, ?], A])
      case ASome(list) => AList[Op[F, ?, ?], B, A](list.reverse)
    }

  def foldLeft[G[_]](ga: G[A])(implicit G: FunctorLike[G, F]): G[B] =
    uncons match {
      case ANone(ev) => ev.subst(ga)
      case ASome(list) => list.foldLeft(ga)
    }

  def foldLeftWhile[G[_], H[_]](ga: G[A])(tr: λ[α => APair[G, F[?, α]]] ~> λ[α => H[α] \/ G[α]]): APair[H, AList[F, ?, B]] \/ G[B] =
    uncons match {
      case ANone(ev) => \/-(ev.subst(ga))
      case ASome(list) => list.foldLeftWhile(ga)(tr)
    }

  def foldRight[G[_]](gb: G[B])(implicit G: ContravariantLike[G, F]): G[A] =
    reverse.foldLeft(gb)(G)
}

object AList {

  type Op[F[_, _], A, B] = F[B, A]
  type Composed[F[_, _], A, B] = AList[Op[F, ?, ?], B, A]

  def apply[F[_, _], A, B](l: AList1[F, A, B]): AList[F, A, B] = new AList(ASome(l))
  def apply[F[_, _], A]: AList[F, A, A] = empty
  def empty[F[_, _], A]: AList[F, A, A] = new AList[F, A, A](AOption.empty)
}
