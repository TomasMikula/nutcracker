package nutcracker

import nutcracker.util.ContU
import scalaz.Leibniz.===
import scalaz.{Bind, Functor, IndexedContT, Leibniz}
import scalaz.syntax.bind._

/** If we are allowed effects `M`, then `Val[A]` can be observed
  * for changes to a (mutable) value of type `A` (for any `A`).
  */
trait Observe[M[_]] extends Observers[M] { self =>
  type Val[_]

  def observeImpl[A, U, Δ](src: Val[A])(f: A => Trigger[A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]
  def observeImplC[A, U, Δ, B](src: Val[A])(f: A => ContU[M, (Trigger[A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)]

  def observe[A](src: Val[A])(implicit dom: Dom[A]): ObserveSyntaxHelper[M, A, dom.Update, dom.Delta, Trigger] =
    new ObserveSyntaxHelper[M, A, dom.Update, dom.Delta, Trigger](observable(src))(dom)

  def peek_[A](ref: Val[A])(f: A => M[Unit])(implicit dom: Dom[A], M: Functor[M]): M[Unit] =
    observe(ref).by(d => fire(f(d))).map((_: Subscription[M]) => ())

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

  def observable[A](src: Val[A]): Observable.Aux[M, A, Trigger] = new DelegateObservers[M, Trigger](self) with Observable[M, A] {
    def observeImpl[U, Δ](f: A => Trigger[A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]] =
      self.observeImpl(src)(f)

    def observeImplC[U, Δ, B](f: A => ContU[M, (Trigger[A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)] =
      self.observeImplC(src)(f)
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
trait Observable[M[_], A] extends Observers[M] { self =>
  type Trigger[a, δ]

  def observeImpl[U, Δ](f: A => Trigger[A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]]
  def observeImplC[U, Δ, B](f: A => ContU[M, (Trigger[A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)]

  def observe(implicit dom: Dom[A]): ObserveSyntaxHelper[M, A, dom.Update, dom.Delta, Trigger] =
    new ObserveSyntaxHelper[M, A, dom.Update, dom.Delta, Trigger](this)(dom)

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB)(implicit M: Functor[M]): Observable[M, B] = new Observable[M, B] {
      type Trigger[b, δb] = (B === b, DB === δb) => self.Trigger[A, DA]

      def observeImpl[U, Δ](h: B => Trigger[B, Δ])(implicit dom: Dom.Aux[B, U, Δ]): M[Subscription[M]] =
        self.observeImpl(a => h(f(a))(Leibniz.refl[B], Dom.relateDeltas(db, dom)))

      def observeImplC[U, Δ, C](h: B => ContU[M, (Trigger[B, Δ], C)])(implicit dom: Dom.Aux[B, U, Δ]): ContU[M, (Subscription[M], C)] =
        self.observeImplC(a => h(f(a)).map({ case (t, c) => (t(Leibniz.refl[B], Dom.relateDeltas(db, dom)), c) }))

      override def discard[b, δb]: (B === b, DB === δb) => self.Trigger[A, DA] =
        (_, _) => self.discard
      override def fire[b, δb](action: M[Unit]): (B === b, DB === δb) => self.Trigger[A, DA] =
        (_, _) => self.fire(action)
      override def sleep[b, δb](next: (b, δb) => (B === b, DB === δb) => self.Trigger[A, DA]): (B === b, DB === δb) => self.Trigger[A, DA] =
        (evb, evδ) => self.sleep((a, δa) => next(evb(f(a)), evδ(g(δa)))(evb, evδ))
      override def fireReload[b, δb](action: M[Unit], next: (b, δb) => (B === b, DB === δb) => self.Trigger[A, DA]): (B === b, DB === δb) => self.Trigger[A, DA] =
        (evb, evδ) => self.fireReload(action, (a, δa) => next(evb(f(a)), evδ(g(δa)))(evb, evδ))
      override def reconsider[b, δb](action: M[(B === b, DB === δb) => self.Trigger[A, DA]]): (B === b, DB === δb) => self.Trigger[A, DA] =
        (evb, evδ) => self.reconsider(action.map(_(evb, evδ)))
    }
  }

  def asCont(implicit fin: Final[A], dom: Dom[A]): IndexedContT[M, Subscription[M], Unit, fin.Out] =
    IndexedContT { whenFinal.exec(_) }

  def whenFinal(implicit fin: Final[A], dom: Dom[A]): WhenFinalSyntaxHelper[fin.Out] =
    new WhenFinalSyntaxHelper(fin)

  final class WhenFinalSyntaxHelper[A0] private[nutcracker](fin: Final.Aux[A, A0])(implicit dom: Dom[A]) {
    def exec(f: A0 => M[Unit]): M[Subscription[M]] =
      observe.by(threshold(a => fin.extract(a) map f))

    def exec0(f: A => M[Unit]): M[Subscription[M]] =
      observe.by(threshold(a =>
        if(fin.isFinal(a)) Some(f(a))
        else None
      ))
  }
}

object Observable {
  type Aux[M[_], A, T[_, _]] = Observable[M, A] { type Trigger[a, δ] = T[a, δ] }
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

final class ObserveSyntaxHelper[M[_], A, U, Δ, Tr[_, _]](src: Observable[M, A] { type Trigger[a, δ] = Tr[a, δ] })(implicit dom: Dom.Aux[A, U, Δ]) {
  def by(f: A => Tr[A, Δ]): M[Subscription[M]] = src.observeImpl(f)
  def by_(f: A => Tr[A, Δ])(implicit M: Functor[M]): M[Unit] = by(f).void

  def byC[B](f: A => ContU[M, (Tr[A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = src.observeImplC(f)
  def byC_[B](f: A => ContU[M, (Tr[A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byC(f).map(_._2)

  def byM[B](f: A => M[(Tr[A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = byC(a => ContU.liftM(f(a)))
  def byM_[B](f: A => M[(Tr[A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byM(f).map(_._2)

  def threshold(f: A => Option[M[Unit]]): M[Subscription[M]] = src.observeImpl(src.threshold(f))
  def threshold_(f: A => Option[M[Unit]])(implicit M: Functor[M]): M[Unit] = threshold(f).void

  def thresholdOpt(f: A => Option[Option[M[Unit]]]): M[Subscription[M]] = src.observeImpl(src.thresholdOpt(f))
  def thresholdOpt_(f: A => Option[Option[M[Unit]]])(implicit M: Functor[M]): M[Unit] = thresholdOpt(f).void

  def untilRight(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Subscription[M]] = src.observeImpl(src.untilRight(f))
  def untilRight_(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Unit] = untilRight(f).void
}

trait Observers[M[_]] {
  type Trigger[A, Δ]

  def discard[A, Δ]: Trigger[A, Δ]
  def sleep[A, Δ](next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ]
  def fire[A, Δ](action: M[Unit]): Trigger[A, Δ]
  def fireReload[A, Δ](action: M[Unit], next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ]
  def reconsider[A, Δ](action: M[Trigger[A, Δ]]): Trigger[A, Δ]

  def observerS[D, Δ, S](s: S)(f: S => TriggerF[M, D, Δ, S])(implicit M: Functor[M]): Trigger[D, Δ] =
    fix(f(s) map (observerS(_)(f)))

  private def fix[A, Δ](t: TriggerF[M, A, Δ, Trigger[A, Δ]]): Trigger[A, Δ] = {
    import TriggerF._
    t match {
      case Discard() => discard
      case Sleep(next) => sleep(next)
      case Fire(action) => fire(action)
      case FireReload(action, next) => fireReload(action, next)
      case Reconsider(cont) => reconsider(cont)
    }
  }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program. */
  def threshold[D, Δ](f: D => Option[M[Unit]]): D => Trigger[D, Δ] =
    d => f(d) match {
      case None => sleep(threshold1(f))
      case Some(k) => fire(k)
    }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program. */
  def threshold1[D, Δ](f: D => Option[M[Unit]]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[D, Δ] = f(d) match {
        case None => sleep(this)
        case Some(k) => fire(k)
      }
    }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program, if any. */
  def thresholdOpt[D, Δ](f: D => Option[Option[M[Unit]]]): D => Trigger[D, Δ] =
    d => f(d) match {
      case None => sleep(thresholdOpt1(f))
      case Some(ko) => ko match {
        case Some(k) => fire(k)
        case None => discard
      }
    }

  /** Keep trying `f` until it returns `Some`. Then fire the returned program, if any. */
  def thresholdOpt1[D, Δ](f: D => Option[Option[M[Unit]]]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[D, Δ] = f(d) match {
        case None => sleep(this)
        case Some(ko) => ko match {
          case Some(k) => fire(k)
          case None => discard
        }
      }
    }

  def untilRight[D, Δ](f: D => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): D => Trigger[D, Δ] =
    d => f(d) match {
      case Left(k) => reconsider(k.as(sleep(untilRight((d, δ) => f(d)))))
      case Right(k) => fire(k)
    }

  def untilRight[D, Δ](f: (D, Δ) => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[D, Δ] = f(d, δ) match {
        case Left(k) => reconsider(k.as(sleep(this)))
        case Right(k) => fire(k)
      }
    }

  def continually[D, Δ](f: D => M[Unit])(implicit M: Functor[M]): D => Trigger[D, Δ] =
    d => reconsider(f(d).as(sleep(continually((d, δ) => f(d)))))

  def continually[D, Δ](f: (D, Δ) => M[Unit])(implicit M: Functor[M]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[D, Δ] = reconsider(f(d, δ).as(sleep(this)))
    }
}

object Observers {
  type Aux[M[_], Tr[_, _]] = Observers[M] { type Trigger[a, δ] = Tr[a, δ] }
}

private[nutcracker] class DelegateObservers[M[_], Tr[_, _]](base: Observers.Aux[M, Tr]) extends Observers[M] {
  type Trigger[a, δ] = Tr[a, δ]

  override def discard[A, Δ]: Tr[A, Δ] = base.discard
  override def sleep[A, Δ](next: (A, Δ) => Tr[A, Δ]): Tr[A, Δ] = base.sleep(next)
  override def fire[A, Δ](action: M[Unit]): Tr[A, Δ] = base.fire(action)
  override def fireReload[A, Δ](action: M[Unit], next: (A, Δ) => Tr[A, Δ]): Tr[A, Δ] = base.fireReload(action, next)
  override def reconsider[A, Δ](action: M[Tr[A, Δ]]): Tr[A, Δ] = base.reconsider(action)
}