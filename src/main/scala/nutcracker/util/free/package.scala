package nutcracker.util

import scalaz.{Applicative, Foldable, Monoid, Traverse}
import scalaz.syntax.applicative._
import scalaz.syntax.traverse0._

package object free {
  type Const[A, B] = A

  implicit def coproductFoldable[F[_]: Foldable, G[_]: Foldable]: Foldable[({ type Out[a] = Either[F[a], G[a]] })#Out] =
    new Foldable[({ type Out[a] = Either[F[a], G[a]] })#Out] with Foldable.FromFoldMap[({ type Out[a] = Either[F[a], G[a]] })#Out] {
      import scalaz.syntax.foldable._
      def foldMap[A, B](fga: Either[F[A], G[A]])(f: A => B)(implicit F: Monoid[B]): B =
        fga match {
          case Left(fa) => fa.foldMap(f)
          case Right(ga) => ga.foldMap(f)
        }
    }

  implicit def coproductTraverse[F[_]: Traverse, G[_]: Traverse]: Traverse[({ type Out[a] = Either[F[a], G[a]] })#Out] =
    new Traverse[({ type Out[a] = Either[F[a], G[a]] })#Out] {
      def traverseImpl[M[_], A, B](fga: Either[F[A], G[A]])(f: A => M[B])(implicit M: Applicative[M]): M[Either[F[B], G[B]]] =
        fga match {
          case Left(fa) => fa.traverse(f).map(Left(_))
          case Right(ga) => ga.traverse(f).map(Right(_))
        }
    }
}
