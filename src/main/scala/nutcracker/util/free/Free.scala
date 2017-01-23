package nutcracker.util.free

import scala.language.higherKinds
import scalaz.{-\/, Applicative, BindRec, Monad, Monoid, Traverse, \/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.monad._

final case class Free[F[_], A] private(unwrap: FreeBind[(Id :++: F)#Out, A]) extends AnyVal {
  private type F1[X] = (Id :++: F)#Out[X]

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    Free(unwrap.flatMap(a => f(a).unwrap))

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Free.point(f(a)))

  def foldMap[M[_]: BindRec: Applicative](f: F ~> M): M[A] =
    unwrap.foldMap(λ[F1 ~> M](_ match {
      case Left(x) => x.point[M]
      case Right(fx) => f(fx)
    }))

  def foldRun[S](s: S, f: λ[α => (S, F[α])] ~> (S, ?)): (S, A) =
    unwrap.foldRun(s, λ[λ[α => (S, F1[α])] ~> (S, ?)]{
      case (s, f1x) => f1x match {
        case Left(x) => (s, x)
        case Right(fx) => f((s, fx))
      }
    })

  def foldRunRecParM[M[_], S1, S2](s1: S1, f: λ[α => (S1, F[α])] ~> λ[α => M[(S1, Free[F, α], S2 => S2) \/ (S2, α)]])(implicit M0: BindRec[M], M1: Applicative[M], S2: Monoid[S2]): M[(S2, A)] = {
    type F1[X] = this.F1[X] // otherwise scalac error: "private type F1 escapes its defining scope ..."
    unwrap.foldRunRecParM[M, S1, S2](s1, λ[λ[α => (S1, F1[α])] ~> λ[α => M[(S1, FreeBind[F1, α], S2 => S2) \/ (S2, α)]]] {
      case (s1, f1x) => f1x match {
        case Left(x) => M1.point(\/.right((S2.zero, x)))
        case Right(fx) => M0.map(f((s1, fx))) {
          case -\/((s1, fx, tr)) => -\/((s1, fx.unwrap, tr))
          case s2x @ \/-((s2, x)) => s2x
        }
      }
    })
  }
}

object Free extends FreeInstances {
  def point[F[_], A](a: A): Free[F, A] = Free(FreeBind.liftF[(Id :++: F)#Out, A](Left(a)))

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Free(FreeBind.liftF[(Id :++: F)#Out, A](Right(fa)))

  def roll[F[_], A](ffa: F[Free[F, A]]): Free[F, A] =
    liftF(ffa).flatMap(x => x)
}

trait FreeInstances extends FreeInstances1 {

  implicit def bindRecMonadInstance[F[_]]: Monad[Free[F, ?]] with BindRec[Free[F, ?]] =
    new Monad[Free[F, ?]] with BindRec[Free[F, ?]] {
      def tailrecM[A, B](a: A)(f: A => Free[F, A \/ B]): Free[F, B] =
        f(a) flatMap {
          case -\/(a) => tailrecM(a)(f)
          case \/-(b) => point(b)
        }

      def point[A](a: => A): Free[F, A] = Free.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
      override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.map(f)
    }
}

trait FreeInstances1 {

  implicit def traverseInstance[F[_]](implicit F: Traverse[F]): Traverse[Free[F, ?]] =
    new Traverse[Free[F, ?]] {
      val impl = FreeBind.traverseInstance[(Id :++: F)#Out](coproductTraverse[Id, F])
      def traverseImpl[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Free[F, B]] =
        G.map(impl.traverse(fa.unwrap)(f))(Free.apply)
    }
}