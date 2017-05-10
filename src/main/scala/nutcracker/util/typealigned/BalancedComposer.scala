package nutcracker.util.typealigned

import nutcracker.util.Aggregator

import scala.annotation.tailrec
import scala.language.higherKinds
import nutcracker.util.typealigned.BalancedComposer.Direction

import scalaz.{Compose, Semigroup}


final class BalancedComposer[F[_, _], A, B, D <: Direction] private(count: Int, stack: AList1[F, A, B]) {
  import BalancedComposer._

  def +:[Z](f: F[Z, A])(implicit F: Compose[F], pre: D =:= Pre): BalancedComposer[F, Z, B, D] =
    add(pair(f, stack), 1, count)

  /** Like `+:`, but to be used in post-compose mode. */
  def :+[Z](f: F[Z, A])(implicit F: Compose[Op[F, ?, ?]], post: D =:= Post): BalancedComposer[F, Z, B, D] =
    add(pair(f, stack), 1, count)(unflip(F))

  /** Reduction when used as pre-composer. */
  def reduceLeft(implicit F: Compose[F], pre: D =:= Pre): F[A, B] =
    stack.reduceLeft

  /** Reduction when used as post-composer. */
  def reduceRight(implicit F: Compose[Op[F, ?, ?]], post: D =:= Post): F[A, B] =
    stack.reduceLeft(unflip(F))

  @tailrec
  private def add[Z](p: APair[F[Z, ?], AList1[F, ?, B]], lcount: Int, rfactor: Int)(implicit F: Compose[F]): BalancedComposer[F, Z, B, D] = {
    // lcount: number of elemnts composed in the left part or the pair
    // rfactor: how many times more elements are there in the right part of the pair (rcount = lcount * rfactor)
    if(rfactor % 2 == 0) new BalancedComposer(lcount * (rfactor + 1), p._1 :: p._2)
    else {
      val (elem, stack) = (p._1, p._2)
      stack.uncons match {
        case Left(f) => assert(rfactor == 1); new BalancedComposer(lcount * 2, AList1(F.compose(f, elem)))
        case Right(ht) => add(pair(F.compose(ht._1, elem), ht._2), lcount * 2, rfactor / 2)
      }
    }
  }

  @inline private def pair[X, Y](e: F[X, Y], l: AList1[F, Y, B]) =
    APair[F[X, ?], AList1[F, ?, B], Y](e, l)
}

object BalancedComposer {
  sealed trait Direction
  sealed trait Pre extends Direction  // linter:ignore UnextendedSealedTrait
  sealed trait Post extends Direction // linter:ignore UnextendedSealedTrait

  def apply[F[_, _], A, B, D <: Direction](f: F[A, B]): BalancedComposer[F, A, B, D] =
    new BalancedComposer(1, AList1(f))

  implicit def contravariantLike[F[_, _], C](implicit F: Compose[F]): ContravariantLike[BalancedPreComposer[F, ?, C], F] = {
    new FunctorLike.FromContravariant[BalancedPreComposer[F, ?, C], F] {
      def contramap[A, B](bin: BalancedPreComposer[F, A, C])(f: B :<= A): BalancedPreComposer[F, B, C] =
        f +: bin
    }
  }

  implicit def functorLike[F[_, _], A](implicit F: Compose[F]): FunctorLike[BalancedPostComposer[F, A, ?], F] = {
    implicit val Fop = flip(F)
    new FunctorLike.FromCovariant[BalancedPostComposer[F, A, ?], F] {
      def map[B, C](bin: BalancedPostComposer[F, A, B])(f: F[B, C]): BalancedPostComposer[F, A, C] =
        bin :+ f
    }
  }

  private def flip[F[_, _]](F: Compose[F]): Compose[Op[F, ?, ?]] =
    new Compose[Op[F, ?, ?]] {
      def compose[A, B, C](f: F[C, B], g: F[B, A]): F[C, A] =
        F.compose(g, f)
    }

  private def unflip[F[_, _]](F: Compose[Op[F, ?, ?]]): Compose[F] =
    flip[Op[F, ?, ?]](F)
}

object BalancedPreComposer {
  def apply[F[_, _], A, B](f: F[A, B]): BalancedPreComposer[F, A, B] = BalancedComposer(f)
}

object BalancedPostComposer {
  import BalancedComposer.Post

  def apply[F[_, _], A, B](f: F[A, B]): BalancedPostComposer[F, A, B] = BalancedComposer[Op[F, ?, ?], B, A, Post](f)
}

final case class BalancedAppender[A] private(repr: BalancedPreComposer[λ[(α, β) => A], Nothing, Nothing]) extends AnyVal {
  def append(a: A)(implicit A: Semigroup[A]): BalancedAppender[A] =
    BalancedAppender((a +: repr)(A.compose, implicitly))

  def result(implicit A: Semigroup[A]): A = repr.reduceLeft(A.compose, implicitly)
}

object BalancedAppender {
  def apply[A](a: A): BalancedAppender[A] =
    BalancedAppender(BalancedPreComposer[λ[(α, β) => A], Nothing, Nothing](a))

  implicit def aggregator[A](implicit A: Semigroup[A]): Aggregator[BalancedAppender[A], A] =
    _ append _
}

final case class BalancedPrepender[A] private(repr: BalancedPostComposer[λ[(α, β) => A], Nothing, Nothing]) extends AnyVal {
  def prepend(a: A)(implicit A: Semigroup[A]): BalancedPrepender[A] =
    BalancedPrepender((repr :+ a)(A.compose, implicitly))

  def result(implicit A: Semigroup[A]): A = repr.reduceRight(A.compose, implicitly)
}

object BalancedPrepender {
  def apply[A](a: A): BalancedPrepender[A] =
    BalancedPrepender(BalancedPostComposer[λ[(α, β) => A], Nothing, Nothing](a))
}