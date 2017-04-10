package nutcracker

import nutcracker.util.ContU
import scalaz.{Bind, Functor, IndexedContT}
import scalaz.syntax.bind._

/** If we are allowed effects `M`, then `S` can be observed for changes to a (mutable) value of type `A`. */
trait Src[M[_], S, A] {

  def observeImpl[U, Δ](src: S)(f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]

  def observe(src: S)(implicit dom: Dom[A]): ObserveSyntaxHelper[dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[dom.Update, dom.Delta](src)(dom)

  final class ObserveSyntaxHelper[U, Δ](src: S)(implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => Trigger[M, A, Δ]): M[Subscription[M]] = observeImpl(src)(f)
  }

  final def source(src: S): Source[M, A] = new Source[M, A] {
    def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]] =
      Src.this.observeImpl(src)(f)
  }

}

/** Polymorphic [[Src]], isomorphic to `∀A. Src[M, F[A], A]`. */
trait PSrc[M[_], F[_]] {

  def observeImpl[A, U, Δ](src: F[A])(f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]
  def observeImplC[A, U, Δ, B](src: F[A])(f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)]

  def observe[A](src: F[A])(implicit dom: Dom[A]): ObserveSyntaxHelper[A, dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[A, dom.Update, dom.Delta](src)(dom)

  final class ObserveSyntaxHelper[A, U, Δ](src: F[A])(implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => Trigger[M, A, Δ]): M[Subscription[M]] = observeImpl(src)(f)
    def by_(f: A => Trigger[M, A, Δ])(implicit M: Functor[M]): M[Unit] = by(f).void

    def byC[B](f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = observeImplC(src)(f)
    def byC_[B](f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byC(f).map(_._2)

    def byM[B](f: A => M[(Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = byC(a => ContU.liftM(f(a)))
    def byM_[B](f: A => M[(Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byM(f).map(_._2)

    def threshold(f: A => Option[M[Unit]]): M[Subscription[M]] = observeImpl(src)(Trigger.threshold(f))
    def threshold_(f: A => Option[M[Unit]])(implicit M: Functor[M]): M[Unit] = threshold(f).void

    def thresholdOpt(f: A => Option[Option[M[Unit]]]): M[Subscription[M]] = observeImpl(src)(Trigger.thresholdOpt(f))
    def thresholdOpt_(f: A => Option[Option[M[Unit]]])(implicit M: Functor[M]): M[Unit] = thresholdOpt(f).void

    def untilRight(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Subscription[M]] = observeImpl(src)(Trigger.untilRight(f))
    def untilRight_(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Unit] = untilRight(f).void
  }

  def peek_[A](ref: F[A])(f: A => M[Unit])(implicit dom: Dom[A], M: Functor[M]): M[Unit] =
    observe(ref).by(d => Trigger.fire(f(d))).map((_: Subscription[M]) => ())

  def alternate[A, B, L, R](ref1: F[A], ref2: F[B])(
    f: (A, B) => Alternator,
    onStartLeft: () => M[L],
    onStartRight: () => M[R],
    onSwitchToLeft: R => M[L],
    onSwitchToRight: L => M[R],
    onStop: Option[Either[L, R]] => M[Unit]
  )(implicit
    domA: Dom[A],
    domB: Dom[B],
    M: Bind[M]
  ): M[Unit] = {
    def observeLeft(b: B, l: L): M[Unit] = observe(ref1).threshold_(a => f(a, b) match {
      case Alternator.Left  => None
      case Alternator.Right => Some(onSwitchToRight(l) >>= { observeRight(a, _) })
      case Alternator.Stop  => Some(onStop(Some(Left(l))))
    })
    def observeRight(a: A, r: R): M[Unit] = observe(ref2).threshold_(b => f(a, b) match {
      case Alternator.Left  => Some(onSwitchToLeft(r) >>= { observeLeft(b, _) })
      case Alternator.Right => None
      case Alternator.Stop  => Some(onStop(Some(Right(r))))
    })
    peek_(ref1)(a => {
      peek_(ref2)(b => {
        f(a, b) match {
          case Alternator.Left  => onStartLeft() >>= { observeLeft(b, _) }
          case Alternator.Right => onStartRight() >>= { observeRight(a, _) }
          case Alternator.Stop  => onStop(None)
        }
      })
    })
  }

  def alternate0[A, B](ref1: F[A], ref2: F[B])(
    f: (A, B) => Alternator,
    onSwitchToLeft: M[Unit],
    onSwitchToRight: M[Unit],
    onStop: M[Unit]
  )(implicit
    domA: Dom[A],
    domB: Dom[B],
    M: Bind[M]
  ): M[Unit] =
    alternate[A, B, Unit, Unit](ref1, ref2)(
      f,
      () => onSwitchToLeft,
      () => onSwitchToRight,
      (_ => onSwitchToLeft),
      (_ => onSwitchToRight),
      (_ => onStop)
    )

  def source[A](src: F[A]): Source[M, A] = new Source[M, A] {
    def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]] =
      PSrc.this.observeImpl(src)(f)
  }
}

/** Relative [[Src]]. Whenever `S` is a source of `A`,
  * then `T` is a source of `B` (under the same effect `M`).
  */
trait RelSrc[S, A, T, B] {
  def apply[M[_]](implicit S: Src[M, S, A]): Src[M, T, B]

  def source(src: T): RelSource[S, A, B] = new RelSource[S, A, B] {
    def apply[M[_]](implicit S: Src[M, S, A]): Source[M, B] = new Source[M, B] {
      def observeImpl[U, Δ](f: B => Trigger[M, B, Δ])(implicit dom: Dom.Aux[B, U, Δ]): M[Subscription[M]] =
        RelSrc.this.apply[M].observeImpl(src)(f)
    }
  }
}

/** Relative [[PSrc]]. Whenever `F` is a polymorphic source,
  * then `G` is also a polymorphic source (under the same effect `M`).
  */
trait RelPSrc[F[_], G[_]] {
  def apply[M[_]](implicit F: PSrc[M, F]): PSrc[M, G]
}

/** OO style source, i.e. data + operations.
  * Can be seen as a [[Src]] instance bundled with (i.e. partially applied to) the argument on which it operates.
  */
trait Source[M[_], A] {
  def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]

  def observe(implicit dom: Dom[A]): ObserveSyntaxHelper[dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[dom.Update, dom.Delta]()(dom)

  final class ObserveSyntaxHelper[U, Δ](implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => Trigger[M, A, Δ]): M[Subscription[M]] = observeImpl(f)
  }

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB)(implicit M: Functor[M]): Source[M, B] = new Source[M, B] {
      def observeImpl[U, Δ](h: B => Trigger[M, B, Δ])(implicit dom: Dom.Aux[B, U, Δ]): M[Subscription[M]] = {
        val g1 = Dom.relateDeltas(db, dom).subst[DA => ?](g)
        Source.this.observeImpl(a => h(f(a)).contramap(f, g1))
      }
    }
  }

  def asCont(implicit fin: Final[A], dom: Dom[A]): IndexedContT[M, Subscription[M], Unit, fin.Out] =
    IndexedContT { whenFinal.exec(_) }

  def whenFinal(implicit fin: Final[A], dom: Dom[A]): WhenFinalSyntaxHelper[fin.Out] =
    new WhenFinalSyntaxHelper(fin)

  final class WhenFinalSyntaxHelper[A0] private[nutcracker](fin: Final.Aux[A, A0])(implicit dom: Dom[A]) {
    def exec(f: A0 => M[Unit]): M[Subscription[M]] =
      observe.by(Trigger.threshold(a => fin.extract(a) map f))

    def exec0(f: A => M[Unit]): M[Subscription[M]] =
      observe.by(Trigger.threshold(a =>
        if(fin.isFinal(a)) Some(f(a))
        else None
      ))
  }
}

/** OO style relative source. */
trait RelSource[S, A, B] {
  def apply[M[_]](implicit S: Src[M, S, A]): Source[M, B]
}

/** OO style source relative to a polymorphic source. */
trait RelPSource[F[_], A] {
  def apply[M[_]](implicit F: PSrc[M, F], M: Functor[M]): Source[M, A]

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB): RelPSource[F, B] = new RelPSource[F, B] {
      def apply[M[_]](implicit F: PSrc[M, F], M: Functor[M]): Source[M, B] = RelPSource.this.apply[M].map(f).deltas(g)
    }
  }

  def asCont[M[_]](implicit fin: Final[A], da: Dom[A], F: PSrc[M, F], M: Functor[M]): IndexedContT[M, Subscription[M], Unit, fin.Out] =
    apply[M].asCont
}

object RelPSource {
  def lift[F[_], A](fa: F[A]): RelPSource[F, A] = new RelPSource[F, A] {
    def apply[M[_]](implicit F: PSrc[M, F], M: Functor[M]): Source[M, A] = F.source(fa)
  }
}