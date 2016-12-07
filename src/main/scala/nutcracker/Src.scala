package nutcracker

import nutcracker.Dom.Aux
import nutcracker.util.ContU

import scala.language.higherKinds

/** If we are allowed effects `M`, then `S` can be observed for changes to a (mutable) value of type `A`. */
trait Src[S, A, M[_]] {

  def observeImpl[U, Δ](src: S)(f: A => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]]))(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def observe(src: S)(implicit dom: Dom[A]): ObserveSyntaxHelper[dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[dom.Update, dom.Delta](src)(dom)

  final class ObserveSyntaxHelper[U, Δ](src: S)(implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]])): M[Unit] = observeImpl(src)(f)
  }

  final def source(src: S): Source[A, M] = new Source[A, M] {
    def observeImpl[U, Δ](f: (A) => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]]))(implicit dom: Dom.Aux[A, U, Δ]): M[Unit] =
      Src.this.observeImpl(src)(f)
  }

}

/** Polymorphic [[Src]], isomorphic to `∀A. Src[F[A], A, M]`. */
trait PSrc[F[_], M[_]] {

  def observeImpl[A, U, Δ](src: F[A])(f: A => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]]))(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def observe[A](src: F[A])(implicit dom: Dom[A]): ObserveSyntaxHelper[A, dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[A, dom.Update, dom.Delta](src)(dom)

  final class ObserveSyntaxHelper[A, U, Δ](src: F[A])(implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]])): M[Unit] = observeImpl(src)(f)
  }

  def source[A](src: F[A]): Source[A, M] = new Source[A, M] {
    def observeImpl[U, Δ](f: (A) => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]]))(implicit dom: Aux[A, U, Δ]): M[Unit] =
      PSrc.this.observeImpl(src)(f)
  }
}

/** Relative [[Src]]. Whenever `S` is a source of `A`,
  * then `T` is a source of `B` (under the same effect `M`).
  */
trait RelSrc[S, A, T, B] {
  def apply[M[_]](implicit S: Src[S, A, M]): Src[T, B, M]

  def source(src: T): RelSource[S, A, B] = new RelSource[S, A, B] {
    def apply[M[_]](implicit S: Src[S, A, M]): Source[B, M] = new Source[B, M] {
      def observeImpl[U, Δ](f: (B) => (Option[M[Unit]], Option[(B, Δ) => Trigger[M[Unit]]]))(implicit dom: Dom.Aux[B, U, Δ]): M[Unit] =
        RelSrc.this.apply[M].observeImpl(src)(f)
    }
  }
}

/** Relative [[PSrc]]. Whenever `F` is a polymorphic source,
  * then `G` is also a polymorphic source (under the same effect `M`).
  */
trait RelPSrc[F[_], G[_]] {
  def apply[M[_]](implicit F: PSrc[F, M]): PSrc[G, M]
}

/** OO style source, i.e. data + operations.
  * Can be seen as a [[Src]] instance bundled with (i.e. partially applied to) the argument on which it operates.
  */
trait Source[A, M[_]] {
  def observeImpl[U, Δ](f: A => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]]))(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def observe(implicit dom: Dom[A]): ObserveSyntaxHelper[dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[dom.Update, dom.Delta]()(dom)

  final class ObserveSyntaxHelper[U, Δ](implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => (Option[M[Unit]], Option[(A, Δ) => Trigger[M[Unit]]])): M[Unit] = observeImpl(f)
  }

  def observeValues(f: A => Trigger[M[Unit]])(implicit dom: Dom[A]): M[Unit] =
    observe.by(a => f(a) match {
      case FireReload(k) => (Some(k), Some((d, δ) => f(d)))
      case Fire(k) => (Some(k), None)
      case Sleep() => (None, Some((d, δ) => f(d)))
      case Discard() => (None, None)
    })

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB): Source[B, M] = new Source[B, M] {
      def observeImpl[U, Δ](h: (B) => (Option[M[Unit]], Option[(B, Δ) => Trigger[M[Unit]]]))(implicit dom: Dom.Aux[B, U, Δ]): M[Unit] =
        Source.this.observeImpl(a => {
          val (now, onChange) = h(f(a))
          (now, onChange map { fn => (a: A, da: DA) => fn(f(a), Dom.relateDeltas(db, dom)(g(da))) })
        })
    }
  }

  def asCont(implicit fin: Final[A], dom: Dom[A]): ContU[M, fin.Out] =
    ContU { whenFinal.exec(_) }

  def whenFinal(implicit fin: Final[A], dom: Dom[A]): WhenFinalSyntaxHelper[fin.Out] =
    new WhenFinalSyntaxHelper(fin)

  final class WhenFinalSyntaxHelper[A0] private[nutcracker](fin: Final.Aux[A, A0])(implicit dom: Dom[A]) {
    def exec(f: A0 => M[Unit]): M[Unit] =
      observeValues(a => fin.extract(a) match {
        case Some(a0) => Fire[M[Unit]](f(a0))
        case None => Sleep[M[Unit]]()
      })

    def exec0(f: A => M[Unit]): M[Unit] =
      observeValues(a =>
        if(fin.isFinal(a)) Fire[M[Unit]](f(a))
        else Sleep[M[Unit]]()
      )
  }
}

/** OO style relative source. */
trait RelSource[S, A, B] {
  def apply[M[_]](implicit S: Src[S, A, M]): Source[B, M]
}

/** OO style source relative to a polymorphic source. */
trait RelPSource[F[_], A] {
  def apply[M[_]](implicit F: PSrc[F, M]): Source[A, M]

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB): RelPSource[F, B] = new RelPSource[F, B] {
      def apply[M[_]](implicit F: PSrc[F, M]): Source[B, M] = RelPSource.this(F).map(f).deltas(g)
    }
  }

  def asCont[M[_]](implicit fin: Final[A], da: Dom[A], F: PSrc[F, M]): ContU[M, fin.Out] =
    apply[M].asCont
}

object RelPSource {
  def lift[F[_], A](fa: F[A]): RelPSource[F, A] = new RelPSource[F, A] {
    def apply[M[_]](implicit F: PSrc[F, M]): Source[A, M] = F.source(fa)
  }
}