package nutcracker.util.free

import scala.annotation.tailrec
import scalaz.{-\/, Applicative, ApplicativePlus, BindRec, Foldable, Monad, MonadPlus, MonadTrans, Monoid, Plus, Traverse, \/, \/-, ~>}
import scalaz.syntax.applicative._

final case class FreeT[F[_], M[_], A] private(unwrap: FreeBind[({ type Out[a] = Either[M[a], F[a]] })#Out, A]) {
  type F1[X] = Either[M[X], F[X]]

  def flatMap[B](f: A => FreeT[F, M, B]): FreeT[F, M, B] =
    FreeT(unwrap.flatMap(a => f(a).unwrap))

  def map[B](f: A => B)(implicit M: Applicative[M]): FreeT[F, M, B] =
    flatMap(a => FreeT.point(f(a)))

  def flatten[A0](implicit ev: A =:= FreeT[F, M, A0]): FreeT[F, M, A0] =
    flatMap[A0](ev)

  def hoist[N[_]](f: M ~> N): FreeT[F, N, A] =
    FreeT(unwrap.mapF[({ type Out[a] = Either[N[a], F[a]] })#Out](Coproduct.injectLeft[M, N, F](f)))

  def interpret[G[_]](f: F ~> G): FreeT[G, M, A] =
    FreeT(unwrap.mapF[({ type Out[a] = Either[M[a], G[a]] })#Out](Coproduct.injectRight[M, F, G](f)))

  def foldMap(f: F ~> M)(implicit M: BindRec[M]): M[A] =
    unwrap.foldMap(
      new (F1 ~> M) {
        override def apply[X](fx: F1[X]): M[X] =
          fx match {
            case Left(mx) => mx
            case Right(fx) => f(fx)
          }
      }
    )

  def foldMapRec(tr: F ~> FreeT[F, M, *])(implicit M: BindRec[M]): M[A] = {
    @tailrec def toM[Z](fa: FreeBind[F1, Z]): M[FreeBind[F1, Z] \/ Z] = {
      fa.resume match {
        case \/-(f1z) => f1z match {
          case Left(mz) => M.map(mz)(\/.right)
          case Right(fz) => toM(tr(fz).unwrap)
        }
        case -\/(p) =>
          val (f1y, f) = (p._1, p._2)
          f1y match {
            case Left(my) => M.map(my)(y => -\/(f(y)))
            case Right(fy) => toM(tr(fy).unwrap.flatMap(f))
          }
      }
    }
    unwrap.foldMapRec[M](
      new (F1 ~> λ[α => M[FreeBind[F1, α] \/ α]]) {
        override def apply[X](fx: F1[X]) =
          fx match {
            case Left(mx) => M.map(mx)(\/.right)
            case Right(fx) => toM(tr(fx).unwrap)
          }
      }
    )
  }

  def cata[B](f: A => B)(implicit F: Foldable[F], M: Foldable[M], B: Monoid[B]): B =
    unwrap.cata(f)

  def traverse[G[_]: Applicative, B](f: A => G[B])(implicit F: Traverse[F], M: Traverse[M]): G[FreeT[F, M, B]] =
    unwrap.traverse[G, B](f).map(FreeT(_))

  def plus(that: FreeT[F, M, A])(implicit M0: Plus[M], M1: BindRec[M], M2: Applicative[M]): FreeT[F, M, A] =
    FreeT.rollM(M0.plus(this.flattenM, that.flattenM))

  def toFree(implicit ev: FreeBind[F1, A] =:= FreeBind[({ type Out[a] = Either[a, F[a]] })#Out, A]): Free[F, A] =
    Free(ev(unwrap))

  /** Flatten all `M`s occuring prior to the first `F`
    * and map the rest of the computation inside it. */
  private def flattenM(implicit M0: BindRec[M], M1: Applicative[M]): M[FreeT[F, M, A]] = {
    def go(ft: FreeT[F, M, A]): M[FreeT[F, M, A] \/ FreeT[F, M, A]] =
      ft.unwrap.handle[M[FreeT[F, M, A] \/ FreeT[F, M, A]]](
        mfa => mfa match {
          case Left(ma) => M0.map(ma)(a => \/-(FreeT.point(a)))
          case Right(fa) => M1.point(\/.right(FreeT.liftF[F, M, A](fa)))
        },
        new (unwrap.Bound ~> Const[M[FreeT[F, M, A] \/ FreeT[F, M, A]], *]) {
          override def apply[X](b: unwrap.Bound[X]): Const[M[FreeT[F, M, A] \/ FreeT[F, M, A]], X] =
            b match {
              case (mfz, f) => mfz match {
                case Left(mz) => M0.map(mz)(z => -\/(FreeT(f(z))))
                case Right(fz) => M1.point(\/-(FreeT.liftBind(fz, f andThen FreeT.apply)))
              }
            }
        }
      )

    M0.tailrecM(this)(go)
  }
}

object FreeT extends FreeTInstances {
  def point[F[_], M[_]: Applicative, A](a: A): FreeT[F, M, A] =
    liftM(a.point[M])

  def liftF[F[_], M[_], A](fa: F[A]): FreeT[F, M, A] = {
    type MF[a] = Either[M[a], F[a]]
    FreeT(FreeBind.liftF[MF, A](Right(fa)))
  }

  def liftM[F[_], M[_], A](ma: M[A]): FreeT[F, M, A] = {
    type MF[a] = Either[M[a], F[a]]
    FreeT(FreeBind.liftF[MF, A](Left(ma)))
  }

  def rollM[F[_], M[_], A](ma: M[FreeT[F, M, A]]): FreeT[F, M, A] =
    liftM[F, M, FreeT[F, M, A]](ma).flatten

  private[FreeT] def liftBind[F[_], M[_], Z, A](fz: F[Z], f: Z => FreeT[F, M, A]): FreeT[F, M, A] =
    liftF(fz).flatMap(f)

  implicit def monadTransInstance[F[_]]: MonadTrans[({ type Out[M[_], A] = FreeT[F, M, A] })#Out] =
    new MonadTrans[({ type Out[M[_], A] = FreeT[F, M, A] })#Out] {
      def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): FreeT[F, M, A] = FreeT.liftM[F, M, A](ma)
      implicit def apply[M[_]](implicit M: Monad[M]): Monad[FreeT[F, M, *]] = monadBindRecInstance
    }
}

private[free] trait FreeTInstances extends FreeTInstances1 {

  implicit def monadPlusInstance[F[_], M[_]: ApplicativePlus: BindRec]: MonadPlus[FreeT[F, M, *]] =
    new MonadPlus[FreeT[F, M, *]] {
      def point[A](a: => A): FreeT[F, M, A] = FreeT.point[F, M, A](a)
      def bind[A, B](fa: FreeT[F, M, A])(f: (A) => FreeT[F, M, B]): FreeT[F, M, B] = fa.flatMap(f)
      def empty[A]: FreeT[F, M, A] = FreeT.liftM[F, M, A](ApplicativePlus[M].empty[A])
      def plus[A](a: FreeT[F, M, A], b: => FreeT[F, M, A]): FreeT[F, M, A] =
        a plus b
    }

}

private[free] trait FreeTInstances1 extends FreeTInstances2 {

  implicit def monadBindRecInstance[F[_], M[_]: Applicative]: Monad[FreeT[F, M, *]] with BindRec[FreeT[F, M, *]] =
    new Monad[FreeT[F, M, *]] with BindRec[FreeT[F, M, *]] {
      def tailrecM[A, B](a: A)(f: A => FreeT[F, M, A \/ B]): FreeT[F, M, B] =
        f(a) flatMap {
          case -\/(a) => tailrecM(a)(f)
          case \/-(b) => FreeT.point[F, M, B](b)
        }

      def point[A](a: => A): FreeT[F, M, A] = FreeT.point[F, M, A](a)
      def bind[A, B](fa: FreeT[F, M, A])(f: (A) => FreeT[F, M, B]): FreeT[F, M, B] = fa.flatMap(f)
    }

}

private[free] trait FreeTInstances2 extends FreeTInstances3 {

  implicit def traverseInstance[F[_] : Traverse, M[_] : Traverse]: Traverse[FreeT[F, M, *]] =
    new Traverse[FreeT[F, M, *]] {
      def traverseImpl[G[_], A, B](fa: FreeT[F, M, A])(f: A => G[B])(implicit G: Applicative[G]): G[FreeT[F, M, B]] =
        fa.traverse(f)
    }
}

private[free] trait FreeTInstances3 extends FreeTInstances4 {

  implicit def foldableInstance[F[_]: Foldable, M[_]: Foldable]: Foldable[FreeT[F, M, *]] =
    new Foldable[FreeT[F, M, *]] with Foldable.FromFoldMap[FreeT[F, M, *]] {
      def foldMap[A, B](fa: FreeT[F, M, A])(f: A => B)(implicit B: Monoid[B]): B =
        fa.cata(f)
    }
}

private[free] trait FreeTInstances4 {

  implicit def plusInstance[F[_], M[_]: Plus: BindRec: Applicative]: Plus[FreeT[F, M, *]] =
    new Plus[FreeT[F, M, *]] {
      def plus[A](a: FreeT[F, M, A], b: => FreeT[F, M, A]): FreeT[F, M, A] =
        a plus b
    }
}
