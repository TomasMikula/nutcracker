package nutcracker.util.free

import scalaz.{-\/, Applicative, BindRec, Monad, Monoid, Traverse, \/, \/-, ~>}
import scalaz.Id.Id
import scalaz.syntax.applicative._

final case class Free[F[_], A](unwrap: FreeBind[(Id :++: F)#Out, A]) extends AnyVal {
  private type F1[X] = (Id :++: F)#Out[X]

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    Free(unwrap.flatMap(a => f(a).unwrap))

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Free.point(f(a)))

  def foldMap[M[_]: BindRec: Applicative](f: F ~> M): M[A] =
    unwrap.foldMap(
      new (F1 ~> M) {
        override def apply[X](fx: F1[X]): M[X] = fx match {
          case Left(x) => x.point[M]
          case Right(fx) => f(fx)
        }
      }
    )

  def foldMapRec[M[_]](f: F ~> λ[α => M[Free[F, α] \/ α]])(implicit M0: BindRec[M], M1: Applicative[M]): M[A] = {
    type F1[X] = this.F1[X] // otherwise getting scalac error: "private type F1 escapes its defining scope as part of type M[scalaz.\/[nutcracker.util.free.FreeBind[Free.this.F1,α],α]]"
    unwrap.foldMapRec(
      new (F1 ~> λ[α => M[FreeBind[F1, α] \/ α]]) {
        override def apply[X](fx: F1[X]) =
          fx match {
            case Left(x) => M1.point(\/-(x))
            case Right(fx) => M1.map(f(fx))(_.leftMap(_.unwrap))
          }
      }
    )
  }

  def foldRun[S](s: S, f: λ[α => (S, F[α])] ~> (S, *)): (S, A) = {
    type SF1[X] = (S, F1[X])

    unwrap.foldRun(
      s,
      new (SF1 ~> (S, *)) {
        override def apply[X](sfx: SF1[X]): (S, X) =
          sfx match {
            case (s, f1x) => f1x match {
              case Left(x) => (s, x)
              case Right(fx) => f((s, fx))
            }
          }
      },
    )
  }

  def foldRunRec[S](s: S, f: λ[α => (S, F[α])] ~> λ[α => (S, Free[F, α], S => S) \/ (S, α)]): (S, A) = {
    type F1[X] = this.F1[X] // otherwise scalac error: "private type F1 escapes its defining scope ..."
    type SF1[X] = (S, F1[X])
    unwrap.foldRunRecM[Id, S](
      s,
      new (SF1 ~> λ[α => (S, FreeBind[F1, α], S => S) \/ (S, α)]) {
        override def apply[X](sfx: SF1[X]) =
          sfx match {
            case (s, f1x) => f1x match {
              case Left(x) => \/-((s, x))
              case Right(fx) => f((s, fx)) match {
                case \/-(sa) => \/-(sa)
                case -\/((s, fx, tr)) => -\/((s, fx.unwrap, tr))
              }
            }
          }
      },
    )(scalaz.Id.id)
  }

  def foldRunRecParM[M[_], S1, S2](
    s1: S1,
    f: λ[α => (S1, F[α])] ~> λ[α => M[(S1, Free[F, α], S2 => S2) \/ (S2, α)]],
  )(implicit
    M0: BindRec[M],
    M1: Applicative[M],
    S2: Monoid[S2],
  ): M[(S2, A)] = {
    type F1[X] = this.F1[X] // otherwise scalac error: "private type F1 escapes its defining scope ..."
    type S1F1[X] = (S1, F1[X])
    unwrap.foldRunRecParM[M, S1, S2](
      s1,
      new (S1F1 ~> λ[α => M[(S1, FreeBind[F1, α], S2 => S2) \/ (S2, α)]]) {
        override def apply[X](sfx: S1F1[X]) =
          sfx match {
            case (s1, f1x) => f1x match {
              case Left(x) => M1.point(\/.right((S2.zero, x)))
              case Right(fx) => M0.map(f((s1, fx))) {
                case -\/((s1, fx, tr)) => -\/((s1, fx.unwrap, tr))
                case s2x @ \/-((s2, x)) => s2x.coerceLeft
              }
            }
          }
      },
    )
  }
}

object Free extends FreeInstances {
  def point[F[_], A](a: A): Free[F, A] = Free(FreeBind.liftF[(Id :++: F)#Out, A](Left(a)))

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Free(FreeBind.liftF[(Id :++: F)#Out, A](Right(fa)))

  def roll[F[_], A](ffa: F[Free[F, A]]): Free[F, A] =
    liftF(ffa).flatMap(x => x)
}

trait FreeInstances extends FreeInstances1 {

  implicit def bindRecMonadInstance[F[_]]: Monad[Free[F, *]] with BindRec[Free[F, *]] =
    new Monad[Free[F, *]] with BindRec[Free[F, *]] {
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

  implicit def traverseInstance[F[_]](implicit F: Traverse[F]): Traverse[Free[F, *]] =
    new Traverse[Free[F, *]] {
      val impl = FreeBind.traverseInstance[(Id :++: F)#Out](coproductTraverse[Id, F](scalaz.Id.id, F))
      def traverseImpl[G[_], A, B](fa: Free[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[Free[F, B]] =
        G.map(impl.traverse(fa.unwrap)(f))(Free.apply)
    }
}
