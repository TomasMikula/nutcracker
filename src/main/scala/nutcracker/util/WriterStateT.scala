package nutcracker.util

import scala.language.higherKinds
import scalaz.{-\/, BindRec, Functor, Lens, Monad, MonadTrans, Monoid, \/, \/-}

final case class WriterStateT[F[_], W, S, A](run: S => F[(W, S, A)]) extends AnyVal {
  def apply(s: S): F[(W, S, A)] = run(s)

  def map[B](f: A => B)(implicit F: Functor[F]): WriterStateT[F, W, S, B] =
    WriterStateT(s => F.map(run(s)) { case (w, s, a) => (w, s, f(a)) })

  def flatMap[B](f: A => WriterStateT[F, W, S, B])(implicit F: Monad[F], W: Monoid[W]): WriterStateT[F, W, S, B] =
    WriterStateT(s => F.bind(run(s)){
      case (w1, s1, a) => F.map(f(a)(s1)) {
        case (w2, s2, b) => (W.append(w1, w2), s2, b)
      }
    })

  def zoomOut[T](l: Lens[T, S])(implicit F: Functor[F]): WriterStateT[F, W, T, A] =
    WriterStateT(t => F.map(run(l.get(t))) { case (w, s, a) => (w, l.set(t, s), a) })
}

object WriterStateT {
  implicit def monad[F[_], W, S](implicit F: Monad[F], W: Monoid[W]): Monad[WriterStateT[F, W, S, ?]] =
    new Monad[WriterStateT[F, W, S, ?]] {
      override def point[A](a: => A): WriterStateT[F, W, S, A] =
        WriterStateT(s => F.point((W.zero, s, a)))
      override def map[A, B](fa: WriterStateT[F, W, S, A])(f: A => B): WriterStateT[F, W, S, B] =
        fa.map(f)
      override def bind[A, B](fa: WriterStateT[F, W, S, A])(f: A => WriterStateT[F, W, S, B]): WriterStateT[F, W, S, B] =
        fa.flatMap(f)
    }

  implicit def bindRec[F[_], W, S](implicit F0: BindRec[F], F1: Monad[F], W: Monoid[W]): BindRec[WriterStateT[F, W, S, ?]] =
    new BindRec[WriterStateT[F, W, S, ?]] {
      def tailrecM[A, B](a: A)(f: A => WriterStateT[F, W, S, A \/ B]): WriterStateT[F, W, S, B] = {
        def go(x: (W, S, A)): F[(W, S, A) \/ (W, S, B)] = {
          val (w, s, a) = x
          F0.map(f(a)(s)) {
            case (w1, s1, \/-(b)) => \/-((W.append(w, w1), s1, b))
            case (w1, s1, -\/(a1)) => -\/((W.append(w, w1), s1, a1))
          }
        }
        WriterStateT(s => F0.tailrecM((W.zero, s, a))(go))
      }

      def bind[A, B](fa: WriterStateT[F, W, S, A])(f: A => WriterStateT[F, W, S, B]): WriterStateT[F, W, S, B] =
        fa.flatMap(f)

      def map[A, B](fa: WriterStateT[F, W, S, A])(f: A => B): WriterStateT[F, W, S, B] =
        fa.map(f)(F0)
    }

  implicit def monadTrans[W, S](implicit W: Monoid[W]): MonadTrans[WriterStateT[?[_], W, S, ?]] =
    new MonadTrans[WriterStateT[?[_], W, S, ?]] {
      def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): WriterStateT[G, W, S, A] =
        WriterStateT(s => G.map(ga)(a => (W.zero, s, a)))

      implicit def apply[G[_]](implicit G: Monad[G]): Monad[WriterStateT[G, W, S, ?]] =
        monad[G, W, S]
    }
}

