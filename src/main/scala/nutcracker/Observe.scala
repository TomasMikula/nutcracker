package nutcracker

import nutcracker.util.{ContU, Forall}
import scalaz.{Bind, Functor, IndexedContT, Leibniz, ===}
import scalaz.syntax.functor._
import scalaz.syntax.bind0._

/** If we are allowed effects `M`, then `Val[A]` can be observed
  * for changes to a (mutable) value of type `A` (for any `A`).
  */
trait Observe[M[_]] extends Observers[M] { self =>
  type IVal[_[_]]

  type Val[A] = IVal[[i] =>> A]

  def iObserve[D[_], Δ[_, _]](src: IVal[D], f: [i] => D[i] => ITrigger[D, Δ, i])(implicit dom: IDom.AuxΔ[D, Δ]): M[Subscription[M]]

  def iObserve[D[_]](src: IVal[D])(using dom: IDom[D]): IObserveSyntaxHelper[D, dom.IDelta] =
    new IObserveSyntaxHelper(src)

  def observeImpl[A, U, Δ](src: Val[A])(f: A => Trigger[A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Subscription[M]] =
    iObserve[[i] =>> A, [i, j] =>> Δ](src, [i] => (a: A) => f(a)[i])

  def observeImplC[A, U, Δ, B](src: Val[A])(f: A => ContU[M, (Trigger[A, Δ], B)])(implicit dom: Dom.Aux[A, U, Δ]): ContU[M, (Subscription[M], B)]

  def observe[A](src: Val[A])(implicit dom: Dom[A]): ObserveSyntaxHelper[A, dom.Delta] =
    new ObserveSyntaxHelper(observable(src))

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

  def observable[A](src: Val[A])(using da: Dom[A]): Observable[A, da.Delta] = new Observable[A, da.Delta] {
    override given dom: Dom.AuxΔ[A, da.Delta] = da

    override def observeImpl(f: A => Trigger[A, da.Delta]): M[Subscription[M]] =
      self.observeImpl(src)(f)

    override def observeImplC[B](f: A => ContU[M, (Trigger[A, da.Delta], B)]): ContU[M, (Subscription[M], B)] =
      self.observeImplC(src)(f)
  }

  class IObserveSyntaxHelper[D[_], Δ[_, _]](src: IVal[D])(using dom: IDom.AuxΔ[D, Δ]) {
    def by(f: [i] => D[i] => ITrigger[D, Δ, i]): M[Subscription[M]] =
      iObserve(src, f)

    def by_(f: [i] => D[i] => ITrigger[D, Δ, i]): M[Unit] =
      by(f).void
  }

  final class ObserveSyntaxHelper[A, Δ](src: Observable[A, Δ])(using dom: Dom.AuxΔ[A, Δ]) {
    def by(f: A => Trigger[A, Δ]): M[Subscription[M]] = src.observeImpl(f)
    def by_(f: A => Trigger[A, Δ]): M[Unit] = by(f).void

    def byC[B](f: A => ContU[M, (Trigger[A, Δ], B)]): ContU[M, (Subscription[M], B)] = src.observeImplC(f)
    def byC_[B](f: A => ContU[M, (Trigger[A, Δ], B)]): ContU[M, B] = byC(f).map(_._2)

    def byM[B](f: A => M[(Trigger[A, Δ], B)])(implicit M: Bind[M]): ContU[M, (Subscription[M], B)] = byC(a => ContU.liftM(f(a)))
    def byM_[B](f: A => M[(Trigger[A, Δ], B)])(implicit M: Bind[M]): ContU[M, B] = byM(f).map(_._2)

    def threshold(f: A => Option[M[Unit]]): M[Subscription[M]] = src.observeImpl(self.threshold(f))
    def threshold_(f: A => Option[M[Unit]]): M[Unit] = threshold(f).void

    def thresholdOpt(f: A => Option[Option[M[Unit]]]): M[Subscription[M]] = src.observeImpl(self.thresholdOpt(f))
    def thresholdOpt_(f: A => Option[Option[M[Unit]]]): M[Unit] = thresholdOpt(f).void

    def thresholdTransition(f: A => Option[Trigger[A, Δ]]): M[Subscription[M]] = src.observeImpl(self.thresholdTransition(f))
    def thresholdTransition_(f: A => Option[Trigger[A, Δ]]): M[Unit] = thresholdTransition(f).void

    def untilRight(f: A => Either[M[Unit], M[Unit]]): M[Subscription[M]] = src.observeImpl(self.untilRight(f))
    def untilRight_(f: A => Either[M[Unit], M[Unit]]): M[Unit] = untilRight(f).void

    def untilRightSeq(f: A => Either[M[Unit], M[Unit]]): M[Subscription[M]] = src.observeImpl(self.untilRightSeq(f))
    def untilRightSeq_(f: A => Either[M[Unit], M[Unit]]): M[Unit] = untilRightSeq(f).void

    def untilRightS[S](init: A => Either[M[S], M[Unit]], trans: (S, A, Δ) => Either[M[S], M[Unit]]): M[Subscription[M]] =
      src.observeImpl(self.untilRightS(init, trans))
  }

  sealed trait Observable[A, Δ] { self =>
    given dom: Dom.AuxΔ[A, Δ]

    def observeImpl(f: A => Trigger[A, Δ]): M[Subscription[M]]
    def observeImplC[B](f: A => ContU[M, (Trigger[A, Δ], B)]): ContU[M, (Subscription[M], B)]

    def observe: ObserveSyntaxHelper[A, Δ] =
      new ObserveSyntaxHelper(this)

    def map[B](f: A => B): MapSyntaxHelper[B] =
      new MapSyntaxHelper(f)

    final class MapSyntaxHelper[B](f: A => B) {
      def deltas[ΔB](g: Δ => ΔB)(implicit db: Dom.AuxΔ[B, ΔB]): Observable[B, ΔB] =
        new Observable[B, ΔB] {
          override given dom: Dom.AuxΔ[B, ΔB] = db

          override def observeImpl(h: B => Trigger[B, ΔB]): M[Subscription[M]] =
            self.observeImpl(a => h(f(a)).contramap(f, g))

          override def observeImplC[C](h: B => ContU[M, (Trigger[B, ΔB], C)]): ContU[M, (Subscription[M], C)] =
            self.observeImplC(a => h(f(a)).map { case (t, c) => (t.contramap(f, g), c) })
        }
    }

    def asCont(implicit fin: Final[A]): IndexedContT[Subscription[M], Unit, M, fin.Out] =
      IndexedContT { whenFinal.exec(_) }

    def whenFinal(implicit fin: Final[A]): WhenFinalSyntaxHelper[fin.Out] =
      new WhenFinalSyntaxHelper(fin)

    final class WhenFinalSyntaxHelper[A0] private[nutcracker](fin: Final.Aux[A, A0]) {
      def exec(f: A0 => M[Unit]): M[Subscription[M]] =
        observe.by(threshold(a => fin.extract(a) map f))

      def exec0(f: A => M[Unit]): M[Subscription[M]] =
        observe.by(threshold(a =>
          if(fin.isFinal(a)) Some(f(a))
          else None
        ))
    }
  }
}

sealed trait Alternator

object Alternator {
  case object Left extends Alternator
  case object Right extends Alternator
  case object Stop extends Alternator
}

/** Relative [[Observe]]: whenever `ObserveVal[M, F]` for some effect `M[_]`,
  * then also `ObserveVal[M, G]`, for the same effect `M`.
  */
trait RelObserve[F[_], G[_]] {
  def apply[M[_]](implicit F: ObserveVal[M, F]): ObserveVal[M, G]
}

trait RelObservable[F[_], A, Δ] {
  def apply[M[_]](implicit M: ObserveVal[M, F]): M.Observable[A, Δ]

  def map[B](f: A => B): MapSyntaxHelper[B] =
    new MapSyntaxHelper(f)

  final class MapSyntaxHelper[B](f: A => B) {
    def deltas[ΔB](g: Δ => ΔB)(using db: Dom.AuxΔ[B, ΔB]): RelObservable[F, B, ΔB] = new RelObservable[F, B, ΔB] {
      override def apply[M[_]](implicit M: ObserveVal[M, F]): M.Observable[B, ΔB] =
        RelObservable.this.apply[M].map(f).deltas(g)
    }
  }

  def asCont[M[_]](implicit fin: Final[A], M: ObserveVal[M, F]): IndexedContT[Subscription[M], Unit, M, fin.Out] =
    apply[M].asCont
}

object RelObservable {
  def lift[F[_], A](fa: F[A])(using dom: Dom[A]): RelObservable[F, A, dom.Delta] =
    new RelObservable[F, A, dom.Delta] {
      override def apply[M[_]](implicit M: ObserveVal[M, F]): M.Observable[A, dom.Delta] =
        M.observable(fa)
    }
}

trait Observers[M[_]] {
  type ITrigger[D[_], Δ[_, _], I]

  final type Trigger[A, Δ] = Forall[ITrigger[[i] =>> A, [i, j] =>> Δ, *]]

  implicit def M: Functor[M]

  def iDiscard[D[_], Δ[_, _], I]: ITrigger[D, Δ, I]
  def iSleep[D[_], Δ[_, _], I](next: [j] => (D[j], Δ[I, j]) => ITrigger[D, Δ, j]): ITrigger[D, Δ, I]
  def iFire[D[_], Δ[_, _], I](action: M[Unit]): ITrigger[D, Δ, I]
  def iFireReload[D[_], Δ[_, _], I](action: M[Unit], next: [j] => (D[j], Δ[I, j]) => ITrigger[D, Δ, j]): ITrigger[D, Δ, I]
  def iReconsider[D[_], Δ[_, _], I](action: M[ITrigger[D, Δ, I]]): ITrigger[D, Δ, I]

  extension [D[_], Δ[_, _], I](t: ITrigger[D, Δ, I]) {
    def contramap[C[_], Γ[_, _]](
      f: [i] => C[i] => D[i],
      g: [i, j] => Γ[i, j] => Δ[i, j],
    ): ITrigger[C, Γ, I]
  }

  extension [D, Δ](t: Trigger[D, Δ]) {
    def contramap[C, Γ](
      f: C => D,
      g: Γ => Δ,
    ): Trigger[C, Γ] =
      new Forall[ITrigger[[i] =>> C, [i, j] =>> Γ, *]] {
        override def compute[I] =
          t[I].contramap(
            [i] => (c: C) => f(c),
            [i, j] => (γ: Γ) => g(γ),
          )
      }
  }

  def discard[A, Δ]: Trigger[A, Δ] =
    new Forall[ITrigger[[i] =>> A, [i, j] =>> Δ, *]] {
      override def compute[I] =
        iDiscard
    }

  def sleep[A, Δ](next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    new Forall[ITrigger[[i] =>> A, [i, j] =>> Δ, *]] {
      override def compute[I] =
        iSleep([j] => (a: A, δ: Δ) => next(a, δ)[j])
    }

  def fire[A, Δ](action: M[Unit]): Trigger[A, Δ] =
    new Forall[ITrigger[[i] =>> A, [i, j] =>> Δ, *]] {
      override def compute[I] =
        iFire(action)
    }

  def fireReload[A, Δ](action: M[Unit], next: (A, Δ) => Trigger[A, Δ]): Trigger[A, Δ] =
    new Forall[ITrigger[[i] =>> A, [i, j] =>> Δ, *]] {
      override def compute[I] =
        iFireReload(action, [j] => (a: A, δ: Δ) => next(a, δ)[j])
    }

  def reconsider[A, Δ](action: M[Trigger[A, Δ]]): Trigger[A, Δ] =
    new Forall[ITrigger[[i] =>> A, [i, j] =>> Δ, *]] {
      override def compute[I] =
        iReconsider(action.map(_[I]))
    }


  def observerS[D, Δ, S](s: S)(f: S => TriggerF[M, D, Δ, S]): Trigger[D, Δ] =
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

  def thresholdTransition[D, Δ](f: D => Option[Trigger[D, Δ]]): D => Trigger[D, Δ] =
    f(_).getOrElse(sleep(thresholdTransition1(f)))

  def thresholdTransition1[D, Δ](f: D => Option[Trigger[D, Δ]]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      override def apply(d: D, δ: Δ): Trigger[D, Δ] = f(d).getOrElse(sleep(this))
    }

  def untilRight[D, Δ](f: D => Either[M[Unit], M[Unit]]): D => Trigger[D, Δ] =
    d => f(d) match {
      case Left(k) => fireReload(k, untilRight((d, δ) => f(d)))
      case Right(k) => fire(k)
    }

  def untilRight[D, Δ](f: (D, Δ) => Either[M[Unit], M[Unit]]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[D, Δ] = f(d, δ) match {
        case Left(k) => fireReload(k, this)
        case Right(k) => fire(k)
      }
    }

  def untilRightSeq[D, Δ](f: D => Either[M[Unit], M[Unit]]): D => Trigger[D, Δ] =
    d => f(d) match {
      case Left(k) => reconsider(k.as(sleep(untilRightSeq((d, δ) => f(d)))))
      case Right(k) => fire(k)
    }

  def untilRightSeq[D, Δ](f: (D, Δ) => Either[M[Unit], M[Unit]]): (D, Δ) => Trigger[D, Δ] =
    new ((D, Δ) => Trigger[D, Δ]) {
      def apply(d: D, δ: Δ): Trigger[D, Δ] = f(d, δ) match {
        case Left(k) => reconsider(k.as(sleep(this)))
        case Right(k) => fire(k)
      }
    }

  def untilRightS[S, D, Δ](init: D => Either[M[S], M[Unit]], trans: (S, D, Δ) => Either[M[S], M[Unit]]): D => Trigger[D, Δ] =
    d => init(d) match {
      case Left(ms) => reconsider(ms.map(s => sleep(untilRightS(s)(trans))))
      case Right(k) => fire(k)
    }

  def untilRightS[S, D, Δ](s: S)(f: (S, D, Δ) => Either[M[S], M[Unit]]): (D, Δ) => Trigger[D, Δ] =
    (d, δ) => f(s, d, δ) match {
      case Left(ms) => reconsider(ms.map(s => sleep(untilRightS(s)(f))))
      case Right(k) => fire(k)
    }

  def iContinually[D[_], Δ[_, _]](f: [i] => D[i] => M[Unit]): [i] => D[i] => ITrigger[D, Δ, i] =
    [i] => (d: D[i]) =>
      iReconsider[D, Δ, i](f(d).as(iSleep(iContinually[D, Δ, i]([j, k] => (d: D[k], δ: Δ[j, k]) => f(d)))))

  def iContinually[D[_], Δ[_, _], I](f: [i, j] => (D[j], Δ[i, j]) => M[Unit]): [j] => (D[j], Δ[I, j]) => ITrigger[D, Δ, j] =
    [j] => (d: D[j], δ: Δ[I, j]) =>
      iReconsider[D, Δ, j](f(d, δ).map(_ => iSleep[D, Δ, j](iContinually[D, Δ, j](f))))

  def continually[D, Δ](f: D => M[Unit]): D => Trigger[D, Δ] = {
    val res0: [i] => D => ITrigger[[x] =>> D, [x, y] =>> Δ, i] =
      iContinually[[i] =>> D, [i, j] =>> Δ]([i] => (d: D) => f(d))

    d => new Trigger[D, Δ] {
      override def compute[I] = res0[I](d)
    }
  }

  def continually[D, Δ](f: (D, Δ) => M[Unit]): (D, Δ) => Trigger[D, Δ] = {
    val res0: [i] => (D, Δ) => ITrigger[[x] =>> D, [x, y] =>> Δ, i] =
      iContinually[[i] =>> D, [i, j] =>> Δ, Any]([i, j] => (d: D, δ: Δ) => f(d, δ))

    (d, δ) => new Trigger[D, Δ] {
      override def compute[I] = res0[I](d, δ)
    }
  }
}
