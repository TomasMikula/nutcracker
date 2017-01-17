package nutcracker.util.free

import scala.annotation.tailrec
import scala.language.higherKinds
import nutcracker.util.typealigned.{AList, ANil, APair, ASome}

import scalaz.Leibniz.===
import scalaz.{-\/, Applicative, BindRec, Foldable, Monad, MonadTrans, Monoid, NaturalTransformation, Traverse, \/, \/-, ~>}
import scalaz.syntax.foldable._
import scalaz.syntax.monad._

sealed abstract class FreeBind[F[_], A] {
  import FreeBind._

  type Bound[X] = (F[X], X => FreeBind[F, A])

  def flatMap[B](f: A => FreeBind[F, B]): FreeBind[F, B] =
    FlatMap(this, f)

  @tailrec
  final def handle[B](onLeaf: F[A] => B, onBind: Bound ~> Const[B, ?]): B =
    this match {
      case LiftF(fa) => onLeaf(fa)
      case FlatMap(fz, f) => fz match {
        case LiftF(fz) => onBind((fz, f))
        case FlatMap(fy, g) => (fy flatMap (y => g(y) flatMap f)).handle(onLeaf, onBind)
      }
    }

  @tailrec
  final def resume: APair[F, ? => FreeBind[F, A]] \/ F[A] =
    this match {
      case LiftF(fa) => \/-(fa)
      case fm @ FlatMap(fz, f) => fz match {
        case LiftF(fz) => -\/(APair[F, ? => FreeBind[F, A], fm.Pivot](fz, f))
        case FlatMap(fy, g) => (fy flatMap (y => g(y) flatMap f)).resume
      }
    }

  def mapF[G[_]](f: F ~> G): FreeBind[G, A] =
    handle(
      fa => LiftF(f(fa)),
      λ[Bound ~> Const[FreeBind[G, A], ?]] {
        case (fz, g) => LiftF(f(fz)) flatMap (z => g(z).mapF[G](f))
      }
    )

  def foldMap[M[_]](f: F ~> M)(implicit M: BindRec[M]): M[A] =
    M.tailrecM(this)(_.handle(
      fa => f(fa) map (\/.right),
      λ[Bound ~> Const[M[FreeBind[F, A] \/ A], ?]] {
        case (fz, g) => f(fz) map { z => \/.left(g(z)) }
      }
    ))

  def foldMapRec[M[_]](tr: F ~> λ[α => M[FreeBind[F, α] \/ α]])(implicit M: BindRec[M]): M[A] = {
    @tailrec def go(ffa: FreeBind[F, A]): M[FreeBind[F, A] \/ A] =
      ffa match {
        case LiftF(fa) => tr(fa)
        case FlatMap(fz, f) => fz match {
          case LiftF(fz) => tr(fz) map {
            case \/-(z) => -\/(f(z))
            case -\/(ffz) => -\/(ffz.flatMap(f))
          }
          case FlatMap(fy, g) => go((fy flatMap (y => g(y) flatMap f)))
        }
      }

    M.tailrecM(this)(go)
  }

  def fold(implicit F: BindRec[F]): F[A] =
    foldMap(NaturalTransformation.refl)

  def foldRunM[M[_], S](s: S, f: λ[α => (S, F[α])] ~> λ[α => M[(S, α)]])(implicit M: BindRec[M]): M[(S, A)] =
    M.tailrecM((s, this)) { case (s, fa) =>
      fa.handle(
        fa => f((s, fa)) map (\/.right),
        λ[Bound ~> Const[M[(S, FreeBind[F, A]) \/ (S, A)], ?]] {
          case (fz, g) => f((s, fz)) map { case (s, z) => \/.left((s, g(z))) }
        }
      )
    }

  def foldRunRecM[M[_], S](s: S, f: λ[α => (S, F[α])] ~> λ[α => M[(S, FreeBind[F, α], S => S) \/ (S, α)]])(implicit M: BindRec[M]): M[(S, A)] = {
    sealed trait Tr[α, β]
    case class StateTr[α, β](tr: S => S, ev: α === β) extends Tr[α, β]
    case class FlatMap[α, β](f: α => FreeBind[F, β]) extends Tr[α, β]
    def stateTr[α](tr: S => S): Tr[α, α] = StateTr(tr, implicitly)

    type X0[β] = APair[FreeBind[F, ?], AList[Tr, ?, β]]
    def X0[α, β](fα: FreeBind[F, α], l: AList[Tr, α, β]): X0[β] = APair[FreeBind[F, ?], AList[Tr, ?, β], α](fα, l)

    def applyTransitions[α, β](s: S, α: α, l: AList[Tr, α, β]): (S, X0[β]) \/ (S, β) =
      l match {
        case ANil(ev) => \/-((s, ev(α)))
        case ASome(l) => l.head match {
          case StateTr(f, ev) => applyTransitions(f(s), ev(α), l.tail) // XXX: non-stack-safe recursion
          case FlatMap(f) => -\/((s, X0[l.A1, β](f(α), l.tail)))
        }
      }

    type X = X0[A]
    def X(fa: FreeBind[F, A]): X = APair[FreeBind[F, ?], AList[Tr, ?, A], A](fa, AList.empty[Tr, A])

    M.tailrecM[(S, X), (S, A)]((s, X(this))) { case (s, x) =>
      x._1.resume match {
        case \/-(fa) => f((s, fa)) map {
          case -\/((s, fa, pop)) => -\/((s, APair[FreeBind[F, ?], AList[Tr, ?, A], x.A](fa, stateTr[x.A](pop) +: x._2)))
          case \/-((s, a)) => x._2 match {
            case ANil(ev) => \/-((s, ev(a)))
            case ASome(l) => applyTransitions(s, a, l.toList)
          }
        }
        case -\/(p) => f((s, p._1)) map {
          case -\/((s, fz, pop)) => -\/((s, APair[FreeBind[F, ?], AList[Tr, ?, A], p.A](fz, stateTr[p.A](pop) +: FlatMap[p.A, x.A](p._2) +: x._2)))
          case \/-((s, z)) => -\/((s, APair[FreeBind[F, ?], AList[Tr, ?, A], x.A](p._2(z), x._2)))
        }
      }
    }
  }

  def foldRunRecParM[M[_], S1, S2](s: S1, f: λ[α => (S1, F[α])] ~> λ[α => M[(S1, FreeBind[F, α], S2 => S2) \/ (S2, α)]])(implicit M: BindRec[M], S2: Monoid[S2]): M[(S2, A)] = {
    import scalaz.syntax.monoid._
    type S = (S1, S2)
    val g = λ[λ[α => (S, F[α])] ~> λ[α => M[(S, FreeBind[F, α], S => S) \/ (S, α)]]] {
      case ((s1, s2), fa) =>
        f((s1, fa)) map {
          case -\/((s1_, fa, tr)) => -\/(((s1_, S2.zero), fa, { case (_, s2_) => (s1, s2 |+| tr(s2_)) }))
          case \/-((s2_, a)) => \/-(((s1, s2 |+| s2_), a))
        }
    }
    foldRunRecM[M, (S1, S2)]((s, S2.zero), g) map { case ((s1, s2), a) => (s2, a) }
  }

  /** foldRunM specialized for `Id` */
  @tailrec final def foldRun[S](s: S, f: λ[α => (S, F[α])] ~> (S, ?)): (S, A) =
    this match {
      case LiftF(fa) => f((s, fa))
      case FlatMap(fz, h) => fz match {
        case LiftF(fz) => f((s, fz)) match { case (s, z) => h(z).foldRun(s, f) }
        case FlatMap(fy, g) => (fy flatMap (y => g(y) flatMap h)).foldRun(s, f)
      }
    }

  /** foldRunM specialized for `S => M[(S, ?)]` */
  def runStateM[M[_], S](s: S)(implicit ev: FreeBind[F, A] =:= FreeBind[λ[α => S => M[(S, α)]], A], M: BindRec[M]): M[(S, A)] = {
    type F0[X] = S => M[(S, X)]

    @tailrec def go(sfa: (S, FreeBind[F0, A])): M[(S, FreeBind[F0, A]) \/ (S, A)] = {
      val (s, fa) = sfa
      fa match {
        case LiftF(f) => f(s) map (\/.right)
        case FlatMap(fz, h) => fz match {
          case LiftF(f) => f(s) map { case (s, z) => \/.left((s, h(z))) }
          case FlatMap(fy, g) => go((s, fy flatMap (y => g(y) flatMap h)))
        }
      }
    }

    M.tailrecM((s, ev(this)))(go)
  }

  /** runStateM specialized for `Id` */
  def runState[S](s: S)(implicit ev: FreeBind[F, A] =:= FreeBind[λ[α => S => (S, α)], A]): (S, A) = {
    type F0[X] = S => (S, X)

    @tailrec def go(s: S, fa: FreeBind[F0, A]): (S, A) =
      fa match {
        case LiftF(f) => f(s)
        case FlatMap(fz, h) => fz match {
          case LiftF(f) => val (s1, z) = f(s); go(s1, h(z))
          case FlatMap(fy, g) => go(s, fy flatMap (y => g(y) flatMap h))
        }
      }

    go(s, ev(this))
  }

  def cata[B](f: A => B)(implicit F: Foldable[F], B: Monoid[B]): B = {
    @tailrec def go(fas: List[FreeBind[F, A]], acc: List[B]): List[B] =
      fas match {
        case fa :: fas => fa match {
          case LiftF(fa) => go(fas, fa.foldMap(f) :: acc)
          case FlatMap(LiftF(fz), g) => go(fz.foldRight(fas)((z, fas) => g(z) :: fas), acc)
          case FlatMap(FlatMap(fy, g), h) => go((fy flatMap (y => g(y) flatMap h)) :: fas, acc)
        }
        case Nil => acc.reverse
      }

    go(List(this), Nil).reduceOption(B.append(_, _)).getOrElse(B.zero)
  }

  // not stack-safe
  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[FreeBind[F, B]] =
    this match {
      case LiftF(fa) => F.traverse(fa)(f).map(FreeBind.liftF[F, B])
      case FlatMap(fz, h) => fz match {
        case LiftF(fz) => F.traverse(fz)(z => h(z).traverse(f)).map(FreeBind.roll[F, B])
        case FlatMap(fy, g) => (fy flatMap (y => g(y) flatMap h)).traverse(f)
      }
    }
}

object FreeBind extends FreeBindInstances {
  private[FreeBind] case class LiftF[F[_], A](fa: F[A]) extends FreeBind[F, A]
  private[FreeBind] case class FlatMap[F[_], Z, A](fz: FreeBind[F, Z], f: Z => FreeBind[F, A]) extends FreeBind[F, A] {
    type Pivot = Z
  }

  def liftF[F[_], A](fa: F[A]): FreeBind[F, A] = LiftF(fa)
  def roll[F[_], A](ffa: F[FreeBind[F, A]]): FreeBind[F, A] = LiftF(ffa).flatMap(identity)
}

trait FreeBindInstances extends FreeBindInstances1 {
  implicit val monadTransInstance: MonadTrans[FreeBind] = new MonadTrans[FreeBind] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): FreeBind[G, A] = FreeBind.liftF(ga)

    implicit def apply[G[_]](implicit G: Monad[G]): Monad[FreeBind[G, ?]] = new Monad[FreeBind[G, ?]] {
      def point[A](a: => A): FreeBind[G, A] = FreeBind.liftF(G.point(a))
      def bind[A, B](fa: FreeBind[G, A])(f: A => FreeBind[G, B]): FreeBind[G, B] = fa.flatMap(f)
    }
  }
}

trait FreeBindInstances1 {
  implicit def traverseInstance[F[_]: Traverse]: Traverse[FreeBind[F, ?]] =
    new Traverse[FreeBind[F, ?]] {
      def traverseImpl[G[_], A, B](fa: FreeBind[F, A])(f: A => G[B])(implicit G: Applicative[G]): G[FreeBind[F, B]] =
        fa.traverse(f)
    }
}