package nutcracker

import nutcracker.Dom.Aux
import nutcracker.util.ContU

import scala.language.higherKinds
import scalaz.{Functor, ~>}
import scalaz.Id._

/** If we are allowed effects `M`, then `S` can be observed for changes to a (mutable) value of type `A`. */
trait Src[S, A, M[_]] {

  def observeImpl[U, Δ](src: S)(f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def observe(src: S)(implicit dom: Dom[A]): ObserveSyntaxHelper[dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[dom.Update, dom.Delta](src)(dom)

  final class ObserveSyntaxHelper[U, Δ](src: S)(implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => Trigger[M, A, Δ]): M[Unit] = observeImpl(src)(f)
  }

  final def source(src: S): Source[A, M] = new Source[A, M] {
    def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Unit] =
      Src.this.observeImpl(src)(f)
  }

}

/** Polymorphic [[Src]], isomorphic to `∀A. Src[F[A], A, M]`. */
trait PSrc[F[_], M[_]] {

  def observeImpl[A, U, Δ](src: F[A])(f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def observe[A](src: F[A])(implicit dom: Dom[A]): ObserveSyntaxHelper[A, dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[A, dom.Update, dom.Delta](src)(dom)

  final class ObserveSyntaxHelper[A, U, Δ](src: F[A])(implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => Trigger[M, A, Δ]): M[Unit] = observeImpl(src)(f)
    def by(f: Id ~> λ[α => (A => TriggerF[M, α])]): M[Unit] = observeImpl(src)(Trigger.valueObserver(f))
    def threshold(f: A => Option[M[Unit]]): M[Unit] = observeImpl(src)(Trigger.threshold(f))
    def untilRight(f: A => Either[M[Unit], M[Unit]])(implicit M: Functor[M]): M[Unit] = observeImpl(src)(Trigger.untilRight(f))
  }

  def source[A](src: F[A]): Source[A, M] = new Source[A, M] {
    def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Aux[A, U, Δ]): M[Unit] =
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
      def observeImpl[U, Δ](f: B => Trigger[M, B, Δ])(implicit dom: Dom.Aux[B, U, Δ]): M[Unit] =
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
  def observeImpl[U, Δ](f: A => Trigger[M, A, Δ])(implicit dom: Dom.Aux[A, U, Δ]): M[Unit]

  def observe(implicit dom: Dom[A]): ObserveSyntaxHelper[dom.Update, dom.Delta] =
    new ObserveSyntaxHelper[dom.Update, dom.Delta]()(dom)

  final class ObserveSyntaxHelper[U, Δ](implicit dom: Dom.Aux[A, U, Δ]) {
    def by(f: A => Trigger[M, A, Δ]): M[Unit] = observeImpl(f)
  }

  def observeValues(f: Id ~> λ[α => (A => TriggerF[M, α])])(implicit dom: Dom[A]): M[Unit] =
    observeImpl[dom.Update, dom.Delta](Trigger.valueObserver(f))(dom)

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB)(implicit M: Functor[M]): Source[B, M] = new Source[B, M] {
      def observeImpl[U, Δ](h: B => Trigger[M, B, Δ])(implicit dom: Dom.Aux[B, U, Δ]): M[Unit] = {
        val g1 = Dom.relateDeltas(db, dom).subst[DA => ?](g)
        Source.this.observeImpl(a => h(f(a)).contramap(f, g1))
      }
    }
  }

  def asCont(implicit fin: Final[A], dom: Dom[A]): ContU[M, fin.Out] =
    ContU { whenFinal.exec(_) }

  def whenFinal(implicit fin: Final[A], dom: Dom[A]): WhenFinalSyntaxHelper[fin.Out] =
    new WhenFinalSyntaxHelper(fin)

  final class WhenFinalSyntaxHelper[A0] private[nutcracker](fin: Final.Aux[A, A0])(implicit dom: Dom[A]) {
    import TriggerF._

    def exec(f: A0 => M[Unit]): M[Unit] =
      observeValues(λ[Id ~> λ[α => (A => TriggerF[M, α])]](α => a => fin.extract(a) match {
        case Some(a0) => Fire(f(a0))
        case None => Sleep(α)
      }))

    def exec0(f: A => M[Unit]): M[Unit] =
      observeValues(λ[Id ~> λ[α => (A => TriggerF[M, α])]](α => a =>
        if(fin.isFinal(a)) Fire(f(a))
        else Sleep(α)
      ))
  }
}

/** OO style relative source. */
trait RelSource[S, A, B] {
  def apply[M[_]](implicit S: Src[S, A, M]): Source[B, M]
}

/** OO style source relative to a polymorphic source. */
trait RelPSource[F[_], A] {
  def apply[M[_]](implicit F: PSrc[F, M], M: Functor[M]): Source[A, M]

  def map[B](f: A => B)(implicit da: Dom[A], db: Dom[B]): MapSyntaxHelper[B, da.Update, da.Delta, db.Update, db.Delta] =
    new MapSyntaxHelper(f)(da, db)

  final class MapSyntaxHelper[B, UA, DA, UB, DB](f: A => B)(implicit da: Dom.Aux[A, UA, DA], db: Dom.Aux[B, UB, DB]) {
    def deltas(g: DA => DB): RelPSource[F, B] = new RelPSource[F, B] {
      def apply[M[_]](implicit F: PSrc[F, M], M: Functor[M]): Source[B, M] = RelPSource.this.apply[M].map(f).deltas(g)
    }
  }

  def asCont[M[_]](implicit fin: Final[A], da: Dom[A], F: PSrc[F, M], M: Functor[M]): ContU[M, fin.Out] =
    apply[M].asCont
}

object RelPSource {
  def lift[F[_], A](fa: F[A]): RelPSource[F, A] = new RelPSource[F, A] {
    def apply[M[_]](implicit F: PSrc[F, M], M: Functor[M]): Source[A, M] = F.source(fa)
  }
}