package nutcracker.util.free

import scala.language.higherKinds
import scalaz.{-\/, Applicative, ApplicativePlus, BindRec, Foldable, Monad, MonadPlus, MonadTrans, Monoid, Plus, Traverse, \/, \/-, ~>}
import scalaz.syntax.monadPlus._

final case class FreeT[F[_], M[_], A] private(unwrap: FreeBind[(M :++: F)#Out, A]) extends AnyVal {
  type F1[X] = (M :++: F)#Out[X]

  def flatMap[B](f: A => FreeT[F, M, B]): FreeT[F, M, B] =
    FreeT(unwrap.flatMap(a => f(a).unwrap))

  def flatten[A0](implicit ev: A =:= FreeT[F, M, A0]): FreeT[F, M, A0] =
    flatMap[A0](ev)

  def hoist[N[_]](f: M ~> N): FreeT[F, N, A] =
    FreeT(unwrap.mapF[(N :++: F)#Out](Coproduct.injectLeft[M, N, F](f)))

  def interpret[G[_]](f: F ~> G): FreeT[G, M, A] =
    FreeT(unwrap.mapF[(M :++: G)#Out](Coproduct.injectRight[M, F, G](f)))

  def foldMap(f: F ~> M)(implicit M: BindRec[M]): M[A] =
    unwrap.foldMap(λ[F1 ~> M](_ match {
      case Left(mx) => mx
      case Right(fx) => f(fx)
    }))

  def cata[B](f: A => B)(implicit F: Foldable[F], M: Foldable[M], B: Monoid[B]): B =
    unwrap.cata(f)

  def traverse[G[_]: Applicative, B](f: A => G[B])(implicit F: Traverse[F], M: Traverse[M]): G[FreeT[F, M, B]] =
    unwrap.traverse[G, B](f).map(FreeT(_))

  def plus(that: FreeT[F, M, A])(implicit M0: Plus[M], M1: BindRec[M], M2: Applicative[M]): FreeT[F, M, A] =
    FreeT.rollM(M0.plus(this.flattenM, that.flattenM))

  /** Flatten all `M`s occuring prior to the first `F`
    * and map the rest of the computation inside it. */
  private def flattenM(implicit M0: BindRec[M], M1: Applicative[M]): M[FreeT[F, M, A]] = {
    def go(ft: FreeT[F, M, A]): M[FreeT[F, M, A] \/ FreeT[F, M, A]] =
      ft.unwrap.handle(
        mfa => mfa match {
          case Left(ma) => M0.map(ma)(a => \/-(FreeT.point(a)))
          case Right(fa) => M1.point(\/.right(FreeT.liftF[F, M, A](fa)))
        },
        λ[unwrap.Bound ~> Const[M[FreeT[F, M, A] \/ FreeT[F, M, A]], ?]] {
          case (mfz, f) => mfz match {
            case  Left(mz) => M0.map(mz)(z => -\/(FreeT(f(z))))
            case Right(fz) => M1.point(\/-(FreeT.liftBind(fz, f andThen FreeT.apply)))
          }
        }
      )

    M0.tailrecM(this)(go)
  }
}

object FreeT extends FreeTInstances {
  def point[F[_], M[_]: Applicative, A](a: A): FreeT[F, M, A] =
    liftM(a.point[M])

  def liftF[F[_], M[_], A](fa: F[A]): FreeT[F, M, A] =
    FreeT(FreeBind.liftF[(M :++: F)#Out, A](Right(fa)))

  def liftM[F[_], M[_], A](ma: M[A]): FreeT[F, M, A] =
    FreeT(FreeBind.liftF[(M :++: F)#Out, A](Left(ma)))

  def rollM[F[_], M[_], A](ma: M[FreeT[F, M, A]]): FreeT[F, M, A] =
    liftM[F, M, FreeT[F, M, A]](ma).flatten

  private[FreeT] def liftBind[F[_], M[_], Z, A](fz: F[Z], f: Z => FreeT[F, M, A]): FreeT[F, M, A] =
    liftF(fz).flatMap(f)

  implicit def monadTransInstance[F[_]]: MonadTrans[FreeT[F, ?[_], ?]] =
    new MonadTrans[FreeT[F, ?[_], ?]] {
      def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): FreeT[F, M, A] = FreeT.liftM[F, M, A](ma)
      implicit def apply[M[_]](implicit M: Monad[M]): Monad[FreeT[F, M, ?]] = monadBindRecInstance
    }
}

private[free] trait FreeTInstances extends FreeTInstances1 {

  implicit def monadPlusInstance[F[_], M[_]: ApplicativePlus: BindRec]: MonadPlus[FreeT[F, M, ?]] =
    new MonadPlus[FreeT[F, M, ?]] {
      def point[A](a: => A): FreeT[F, M, A] = FreeT.point[F, M, A](a)
      def bind[A, B](fa: FreeT[F, M, A])(f: (A) => FreeT[F, M, B]): FreeT[F, M, B] = fa.flatMap(f)
      def empty[A]: FreeT[F, M, A] = FreeT.liftM[F, M, A](ApplicativePlus[M].empty[A])
      def plus[A](a: FreeT[F, M, A], b: => FreeT[F, M, A]): FreeT[F, M, A] =
        a plus b
    }

}

private[free] trait FreeTInstances1 extends FreeTInstances2 {

  implicit def monadBindRecInstance[F[_], M[_]: Applicative]: Monad[FreeT[F, M, ?]] with BindRec[FreeT[F, M, ?]] =
    new Monad[FreeT[F, M, ?]] with BindRec[FreeT[F, M, ?]] {
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

  implicit def traverseInstance[F[_] : Traverse, M[_] : Traverse]: Traverse[FreeT[F, M, ?]] =
    new Traverse[FreeT[F, M, ?]] {
      def traverseImpl[G[_], A, B](fa: FreeT[F, M, A])(f: A => G[B])(implicit G: Applicative[G]): G[FreeT[F, M, B]] =
        fa.traverse(f)
    }
}

private[free] trait FreeTInstances3 extends FreeTInstances4 {

  implicit def foldableInstance[F[_]: Foldable, M[_]: Foldable]: Foldable[FreeT[F, M, ?]] =
    new Foldable[FreeT[F, M, ?]] with Foldable.FromFoldMap[FreeT[F, M, ?]] {
      def foldMap[A, B](fa: FreeT[F, M, A])(f: A => B)(implicit B: Monoid[B]): B =
        fa.cata(f)
    }
}

private[free] trait FreeTInstances4 {

  implicit def plusInstance[F[_], M[_]: Plus: BindRec: Applicative]: Plus[FreeT[F, M, ?]] =
    new Plus[FreeT[F, M, ?]] {
      def plus[A](a: FreeT[F, M, A], b: => FreeT[F, M, A]): FreeT[F, M, A] =
        a plus b
    }
}