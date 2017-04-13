package nutcracker

import nutcracker.util.ContU
import scalaz.{Bind, Functor, IndexedContT}
import scalaz.syntax.bind._

/** If we are allowed effects `M`, then `Val[A]` can be observed
  * for changes to a (mutable) value of type `A` (for any `A`).
  */
trait Observe[M[_]] {
  type Val[_]

  def observeImpl[A, U, Δ](src: Val[A])(f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]
  def observeImplC[A, U, Δ, B](src: Val[A])(f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)]

  def observe[A](src: Val[A])(implicit dom: Dom[A]): ObserveSyntaxHelper[M, A, dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[M, A, dom.Update, dom.Delta](observable(src))(dom)

  def peek_[A](ref: Val[A])(f: A => M[Unit])(implicit dom: Dom[A], M: Functor[M]): M[Unit] =
    observe(ref).by(d => Trigger.fire(f(d))).map((_: Subscription[M]) => ())

  def alternate[A, B, L, R](ref1: Val[A], ref2: Val[B])(
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

  def alternate0[A, B](ref1: Val[A], ref2: Val[B])(
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

  def observable[A](src: Val[A]): Observable[M, A] = new Observable[M, A] {
    def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]] =
      Observe.this.observeImpl(src)(f)

    def observeImplC[U, Δ, B](f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)] =
      Observe.this.observeImplC(src)(f)
  }
}

/** Relative [[Observe]]: whenever `ObserveVal[M, F]` for some effect `M[_]`,
  * then also `ObserveVal[M, G]`, for the same effect `M`.
  */
trait RelObserve[F[_], G[_]] {
  def apply[M[_]](implicit F: ObserveVal[M, F]): ObserveVal[M, G]
}

/** Can be observed for changes to a (mutable) value of type `A`, under effects `M`.
  * Can be seen as an `F[A]` bundled with an instance of `Observe[M, F]`, for some `F[_]`.
  */
trait Observable[M[_], A] {
  def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]
  def observeImplC[U, Δ, B](f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)]

  def observe(implicit dom: Dom[A]): ObserveSyntaxHelper[M, A, dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[M, A, dom.Update, dom.Delta](this)(dom)

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB)(implicit M: Functor[M]): Observable[M, B] = new Observable[M, B] {
      def observeImpl[U, Δ](h: B => Trigger[M, B, Δ])(implicit dom: Dom.Aux[B, U, Δ]): M[Subscription[M]] = {
        val g1 = Dom.relateDeltas(db, dom).subst[DA => ?](g)
        Observable.this.observeImpl(a => h(f(a)).contramap(f, g1))
      }

      def observeImplC[U, Δ, C](h: B => ContU[M, (Trigger[M, B, Δ], C)])(implicit dom: Dom.Aux[B, U, Δ]): ContU[M, (Subscription[M], C)] = {
        val g1 = Dom.relateDeltas(db, dom).subst[DA => ?](g)
        Observable.this.observeImplC(a => h(f(a)).map({ case (t, c) => (t.contramap(f, g1), c) }))
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

/** Relative [[Observable]]: whenever `Observe[M, F]`, then this can be converted to `Observable[M, A]`. */
trait RelObservable[F[_], A] {
  def apply[M[_]](implicit F: ObserveVal[M, F], M: Functor[M]): Observable[M, A]

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB): RelObservable[F, B] = new RelObservable[F, B] {
      def apply[M[_]](implicit F: ObserveVal[M, F], M: Functor[M]): Observable[M, B] = RelObservable.this.apply[M].map(f).deltas(g)
    }
  }

  def asCont[M[_]](implicit fin: Final[A], da: Dom[A], F: ObserveVal[M, F], M: Functor[M]): IndexedContT[M, Subscription[M], Unit, fin.Out] =
    apply[M].asCont
}

object RelObservable {
  def lift[F[_], A](fa: F[A]): RelObservable[F, A] = new RelObservable[F, A] {
    def apply[M[_]](implicit F: ObserveVal[M, F], M: Functor[M]): Observable[M, A] = F.observable(fa)
  }
}

final class ObserveSyntaxHelper[M[_], A, U, Δ](src: Observable[M, A])(implicit dom: Dom.Aux[A, U, Δ]) {
  def by(f: A => Trigger[M, A, Δ]): M[Subscription[M]] = src.observeImpl(f)
  def by_(f: A => Trigger[M, A, Δ])(implicit M: Functor[M]): M[Unit] = by(f).void

  def byC[B](f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = src.observeImplC(f)
  def byC_[B](f: A => ContU[M, (Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byC(f).map(_._2)

  def byM[B](f: A => M[(Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = byC(a => ContU.liftM(f(a)))
  def byM_[B](f: A => M[(Trigger[M, A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byM(f).map(_._2)

  def threshold(f: A => Option[M[Unit]]): M[Subscription[M]] = src.observeImpl(Trigger.threshold(f))
  def threshold_(f: A => Option[M[Unit]])(implicit M: Functor[M]): M[Unit] = threshold(f).void

  def thresholdOpt(f: A => Option[Option[M[Unit]]]): M[Subscription[M]] = src.observeImpl(Trigger.thresholdOpt(f))
  def thresholdOpt_(f: A => Option[Option[M[Unit]]])(implicit M: Functor[M]): M[Unit] = thresholdOpt(f).void

  def untilRight(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Subscription[M]] = src.observeImpl(Trigger.untilRight(f))
  def untilRight_(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Unit] = untilRight(f).void
}