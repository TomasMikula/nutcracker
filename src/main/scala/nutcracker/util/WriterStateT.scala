package nutcracker.util

import scalaz.{-\/, Applicative, Bind, BindRec, Functor, Lens, Monad, MonadTrans, Monoid, StateT, \/, \/-, ~>}

final case class WriterStateT[W, S, F[_], A](run: S => F[(W, S, A)]) extends AnyVal {
  import WriterStateT._

  def apply(s: S): F[(W, S, A)] = run(s)

  def map[B](f: A => B)(implicit F: Functor[F]): WriterStateT[W, S, F, B] =
    WriterStateT(s => F.map(run(s)) { case (w, s, a) => (w, s, f(a)) })

  def flatMap[B](f: A => WriterStateT[W, S, F, B])(implicit F: Bind[F], W: Monoid[W]): WriterStateT[W, S, F, B] =
    WriterStateT(s => F.bind(run(s)){
      case (w1, s1, a) => F.map(f(a)(s1)) {
        case (w2, s2, b) => (W.append(w1, w2), s2, b)
      }
    })

  def runRec(s: S)(f: W => Option[(WriterStateT[W, S, F, Unit], W)])(implicit F0: BindRec[F], F1: Applicative[F], W: Monoid[W]): F[(S, A)] =
    F0.bind(run(s)) { case (w, s, a) => F0.map(unfold(w, f)(s))((_, a)) }

  def recurse(f: W => Option[(WriterStateT[W, S, F, Unit], W)])(implicit F0: BindRec[F], F1: Monad[F], W: Monoid[W]): StateT[S, F, A] =
    StateT(s => runRec(s)(f))

  def zoomOut[T](implicit l: Lens[T, S], F: Functor[F]): WriterStateT[W, T, F, A] =
    WriterStateT(t => F.map(run(l.get(t))) { case (w, s, a) => (w, l.set(t, s), a) })
}

object WriterStateT extends WriterStateTInstances {
  def unfold[F[_], W, S](w: W, f: W => Option[(WriterStateT[W, S, F, Unit], W)])(s: S)(implicit F0: BindRec[F], F1: Applicative[F], W: Monoid[W]): F[S] = {
    def go(ws: (W, S)): F[(W, S) \/ S] = {
      val (w, s) = ws
      f(w) match {
        case Some((prg, w)) => F0.map(prg.run(s)) { case (w1, s, _) => -\/((W.append(w1, w), s)) }
        case None => F1.point(\/-(s))
      }
    }

    F0.tailrecM((w, s))(go)
  }

  def unfold[F[_], G[_], W0, S](gw: G[W0], f: W0 => WriterStateT[G[W0], S, F, Unit])(s: S)(implicit F0: BindRec[F], F1: Applicative[F], G: Catenable[G]): F[S] =
    unfold[F, G[W0], S](gw, gw => G.uncons(gw).map({ case (w0, gw) => (f(w0), gw) }))(s)(F0, F1, G.monoid[W0])

  def recurse[H[_], F[_], G[_], W0, S](f: H ~> WriterStateT[G[W0], S, F, *])(wh: W0 => H[Unit])(implicit F0: BindRec[F], F1: Monad[F], G: Catenable[G]): H ~> StateT[S, F, *] =
    new (H ~> StateT[S, F, *]) {
      override def apply[A](ha: H[A]): StateT[S, F, A] =
        f(ha).recurse(gw => G.uncons(gw).map({ case (w0, gw) => (f(wh(w0)), gw) }))(F0, F1, G.monoid[W0])
    }
}

trait WriterStateTInstances extends WriterStateTInstances1 {
  implicit def monadTellStateInstance[F[_], W, S](implicit F: Monad[F], W: Monoid[W]): MonadTellState[WriterStateT[W, S, F, *], W, S] =
    new WriterStateTMonad[F, W, S] with MonadTellState[WriterStateT[W, S, F, *], W, S] {
      def writerState[A](f: S => (W, S, A)): WriterStateT[W, S, F, A] =
        WriterStateT(s => F.point(f(s)))

      def get: WriterStateT[W, S, F, S] =
        WriterStateT(s => F.point((W.zero, s, s)))

      def put(s: S): WriterStateT[W, S, F, Unit] =
        WriterStateT(_ => F.point((W.zero, s, ())))

      def writer[A](w: W, a: A): WriterStateT[W, S, F, A] =
        WriterStateT(s => F.point((w, s, a)))
    }

  implicit def monadTrans[W, S](implicit W: Monoid[W]): MonadTrans[({ type Out[M[_], A] = WriterStateT[W, S, M, A]})#Out] =
    new MonadTrans[({ type Out[M[_], A] = WriterStateT[W, S, M, A]})#Out] {
      def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): WriterStateT[W, S, G, A] =
        WriterStateT(s => G.map(ga)(a => (W.zero, s, a)))

      implicit def apply[G[_]](implicit G: Monad[G]): Monad[WriterStateT[W, S, G, *]] =
        monad[G, W, S]
    }
}

trait WriterStateTInstances1 extends WriterStateTInstances2 {
  implicit def bindRec[F[_], W, S](implicit F0: BindRec[F], W: Monoid[W]): BindRec[WriterStateT[W, S, F, *]] =
    new BindRec[WriterStateT[W, S, F, *]] {
      def tailrecM[A, B](a: A)(f: A => WriterStateT[W, S, F, A \/ B]): WriterStateT[W, S, F, B] = {
        def go(x: (W, S, A)): F[(W, S, A) \/ (W, S, B)] = {
          val (w, s, a) = x
          F0.map(f(a)(s)) {
            case (w1, s1, \/-(b)) => \/-((W.append(w, w1), s1, b))
            case (w1, s1, -\/(a1)) => -\/((W.append(w, w1), s1, a1))
          }
        }
        WriterStateT(s => F0.tailrecM((W.zero, s, a))(go))
      }

      def bind[A, B](fa: WriterStateT[W, S, F, A])(f: A => WriterStateT[W, S, F, B]): WriterStateT[W, S, F, B] =
        fa.flatMap(f)

      def map[A, B](fa: WriterStateT[W, S, F, A])(f: A => B): WriterStateT[W, S, F, B] =
        fa.map(f)(F0)
    }
}

trait WriterStateTInstances2 {
  implicit def monad[F[_], W, S](implicit F: Monad[F], W: Monoid[W]): Monad[WriterStateT[W, S, F, *]] =
    new WriterStateTMonad[F, W, S]
}

private[util] class WriterStateTMonad[F[_], W, S](implicit F: Monad[F], W: Monoid[W]) extends Monad[WriterStateT[W, S, F, *]] {
  def point[A](a: => A): WriterStateT[W, S, F, A] =
    WriterStateT(s => F.point((W.zero, s, a)))

  def bind[A, B](fa: WriterStateT[W, S, F, A])(f: A => WriterStateT[W, S, F, B]): WriterStateT[W, S, F, B] =
    fa.flatMap(f)

  override def map[A, B](fa: WriterStateT[W, S, F, A])(f: A => B): WriterStateT[W, S, F, B] =
    fa.map(f)
}