package nutcracker.util.typealigned

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.Compose


final class Binary[F[_, _], A, B] private(count: Int, stack: AList1[F, A, B]) {
  import Binary._

  def +:[Z](f: F[Z, A])(implicit F: Compose[F]): Binary[F, Z, B] =
    add(pair(f, stack), 1, count)

  /** Alias for `+:`, more convenient when used with `LBinary` alias. */
  def :+[Z](f: F[Z, A])(implicit F: Compose[F]): Binary[F, Z, B] =
    f +: this

  def extract(implicit F: Compose[F]): F[A, B] =
    stack.reduceLeft

  def extractL(implicit F: Compose[Op[F, ?, ?]]): F[A, B] =
    extract(unflip(F))

  @tailrec
  private def add[Z](p: APair[F[Z, ?], AList1[F, ?, B]], lcount: Int, rfactor: Int)(implicit F: Compose[F]): Binary[F, Z, B] = {
    // lcount: number of elemnts composed in the left part or the pair
    // rfactor: how many times more elements are there in the right part of the pair (rcount = lcount * rfactor)
    if(rfactor % 2 == 0) new Binary(lcount * (rfactor + 1), p._1 :: p._2)
    else {
      val (elem, stack) = (p._1, p._2)
      stack.uncons match {
        case Left(f) => assert(rfactor == 1); new Binary(lcount * 2, AList1(F.compose(f, elem)))
        case Right(ht) => add(pair(F.compose(ht._1, elem), ht._2), lcount * 2, rfactor / 2)
      }
    }
  }

  @inline private def pair[X, Y](e: F[X, Y], l: AList1[F, Y, B]) =
    APair[F[X, ?], AList1[F, ?, B], Y](e, l)
}

object Binary {

  def apply[F[_, _], A, B](f: F[A, B]): Binary[F, A, B] =
    new Binary(1, AList1(f))

  implicit def contravariantLike[F[_, _], C](implicit F: Compose[F]): ContravariantLike[Binary[F, ?, C], F] = {
    new FunctorLike.FromContravariant[Binary[F, ?, C], F] {
      def contramap[A, B](bin: Binary[F, A, C])(f: B :<= A): Binary[F, B, C] =
        f +: bin
    }
  }

  implicit def functorLike[F[_, _], A](implicit F: Compose[F]): FunctorLike[LBinary[F, A, ?], F] = {
    implicit val Fop = flip(F)
    new FunctorLike.FromCovariant[LBinary[F, A, ?], F] {
      def map[B, C](bin: LBinary[F, A, B])(f: F[B, C]): LBinary[F, A, C] =
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

object RBinary {
  def apply[F[_, _], A, B](f: F[A, B]): RBinary[F, A, B] = Binary(f)
}

object LBinary {
  def apply[F[_, _], A, B](f: F[A, B]): LBinary[F, A, B] = Binary[Op[F, ?, ?], B, A](f)
}
