package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK, StateInterpreter}
import shapeless.HList

import scalaz.{Functor, ~>}

sealed trait PropagationLang[Ref[_], K[_], A] {
  protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, L, A]
}

object PropagationLang {

  private type FP[Ref[_], A] = FreeK[PropagationLang[Ref, ?[_], ?], A]

  // constructors (the instruction set of a free program)
  case class NewCell[Ref[_], K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Ref[D]] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, L, Ref[D]] = NewCell(d, dom)
  }
  case class Update[Ref[_], K[_], D, U, Δ](ref: Ref[D], u: U, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, L, Unit] = Update(ref, u, dom)
  }
  case class Observe[Ref[_], K[_], D, U, Δ](ref: Ref[D], f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K[Unit]]]), dom: Dom.Aux[D, U, Δ]) extends PropagationLang[Ref, K, Unit] {
    protected def transform[L[_]](tr: K ~> L): PropagationLang[Ref, L, Unit] = {

      def ftr[A, B](f: (A, B) => Trigger[K[Unit]], tr: K ~> L)(implicit ft: Functor[Trigger]): (A, B) => Trigger[L[Unit]] =
        (a, b) => ft.map(f(a, b))(tr[Unit] _)

      observe(ref){ d =>
        val (now, onChange) = f(d)
        (now map (tr(_)), onChange map (action => ftr(action, tr)))
      }(dom)
    }
  }
  case class SelTrigger[Ref[_], K[_], L <: HList](sel: Sel[Ref, L], f: L => Trigger[K[Unit]]) extends PropagationLang[Ref, K, Unit] {
    protected def transform[K2[_]](tr: K ~> K2): PropagationLang[Ref, K2, Unit] = selTrigger(sel){ l => Functor[Trigger].map(f(l))(tr[Unit] _) }
  }

  // constructors returning less specific types, and curried to help with type inference
  def newCell[Ref[_], K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[Ref, K, Ref[D]] =
    NewCell[Ref, K, D, dom.Update, dom.Delta](d, dom)
  def update[Ref[_], K[_], D, U, Δ](ref: Ref[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): PropagationLang[Ref, K, Unit] =
    Update[Ref, K, D, U, Δ](ref, u, dom)
  def observe[Ref[_], K[_], D, U, Δ](ref: Ref[D])(f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K[Unit]]]))(implicit dom: Dom.Aux[D, U, Δ]): PropagationLang[Ref, K, Unit] =
    Observe[Ref, K, D, U, Δ](ref, f, dom)
  def selTrigger[Ref[_], K[_], L <: HList](sel: Sel[Ref, L])(f: L => Trigger[K[Unit]]): PropagationLang[Ref, K, Unit] =
    SelTrigger(sel, f)


  implicit def functorKInstance[Ref[_]]: FunctorKA[PropagationLang[Ref, ?[_], ?]] = new FunctorKA[PropagationLang[Ref, ?[_], ?]] {
    def transform[K[_], L[_], A](pk: PropagationLang[Ref, K, A])(tr: K ~> L): PropagationLang[Ref, L, A] = pk.transform(tr)
  }

  implicit def freePropagation[Ref[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]): Propagation[FreeK[F, ?], Ref] =
    new FreePropagation[Ref, F]

  implicit def interpreter[Ref[_]]: StateInterpreter[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?]] =
    PropagationStore.interpreter
}


private[nutcracker] class FreePropagation[Ref[_], F[_[_], _]](implicit inj: InjectK[PropagationLang[Ref, ?[_], ?], F]) extends Propagation[FreeK[F, ?], Ref] {

  def newCell[D](d: D)(implicit dom: Dom[D]): FreeK[F, Ref[D]] =
    FreeK.injLiftF(PropagationLang.newCell[Ref, FreeK[F, ?], D](d))

  def updateImpl[D, U, Δ](ref: Ref[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.update[Ref, FreeK[F, ?], D, U, Δ](ref)(u))

  def observeImpl[D, U, Δ](ref: Ref[D])(f: D => (Option[FreeK[F, Unit]], Option[(D, Δ) => Trigger[FreeK[F, Unit]]]))(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.observe[Ref, FreeK[F, ?], D, U, Δ](ref)(f))

  def selTrigger[L <: HList](sel: Sel[Ref, L])(f: (L) => Trigger[FreeK[F, Unit]]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.selTrigger[Ref, FreeK[F, ?], L](sel)(f))
}