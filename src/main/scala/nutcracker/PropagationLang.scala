package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK, StateInterpreter}
import shapeless.HList

import scalaz.{Functor, ~>}

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  // constructors (the instruction set of a free program)
  case class Cell[K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, DRef[D]]
  case class Update[K[_], D, U, Δ](ref: DRef[D], u: U, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]
  case class Observe[K[_], D, U, Δ](ref: DRef[D], f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K[Unit]]]), dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[DRef, L], f: L => Trigger[K[Unit]]) extends PropagationLang[K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def cell[K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[K, DRef[D]] =
    Cell[K, D, dom.Update, dom.Delta](d, dom)
  def update[K[_], D, U, Δ](ref: DRef[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): PropagationLang[K, Unit] =
    Update[K, D, U, Δ](ref, u, dom)
  def observe[K[_], D, U, Δ](ref: DRef[D])(f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K[Unit]]]))(implicit dom: Dom.Aux[D, U, Δ]): PropagationLang[K, Unit] =
    Observe[K, D, U, Δ](ref, f, dom)
  def selTrigger[K[_], L <: HList](sel: Sel[DRef, L])(f: L => Trigger[K[Unit]]): PropagationLang[K, Unit] =
    SelTrigger(sel, f)


  implicit def functorKInstance: FunctorKA[PropagationLang] = new FunctorKA[PropagationLang] {

    private def ftr[A, B, K[_], L[_]](f: (A, B) => Trigger[K[Unit]], tr: K ~> L)(implicit ft: Functor[Trigger]): (A, B) => Trigger[L[Unit]] =
      (a, b) => ft.map(f(a, b))(tr[Unit] _)

    def transform[K[_], L[_], A](pk: PropagationLang[K, A])(tr: K ~> L): PropagationLang[L, A] = pk match {

      // the interesting cases
      case Observe(ref, f, dom) => observe(ref){ d =>
        val (now, onChange) = f(d)
        (now map (tr(_)), onChange map (action => ftr(action, tr)))
      }(dom)
      case SelTrigger(sel, f)   => selTrigger(sel){ l => Functor[Trigger].map(f(l))(tr[Unit] _) }

      // the boring cases
      case Cell(d, dom)        => Cell(d, dom)
      case Update(ref, u, dom) => Update(ref, u, dom)
    }
  }

  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[PropagationLang, F]): Propagation[FreeK[F, ?], DRef] =
    new FreePropagation[F]

  implicit def interpreter: StateInterpreter[PropagationLang, PropagationStore] = PropagationStore.interpreter
}


private[nutcracker] class FreePropagation[F[_[_], _]](implicit inj: InjectK[PropagationLang, F]) extends Propagation[FreeK[F, ?], DRef] {

  def cell[D](d: D)(implicit dom: Dom[D]): FreeK[F, DRef[D]] =
    FreeK.injLiftF(PropagationLang.cell[FreeK[F, ?], D](d))

  def updateImpl[D, U, Δ](ref: DRef[D])(u: U)(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.update[FreeK[F, ?], D, U, Δ](ref)(u))

  def observeImpl[D, U, Δ](ref: DRef[D])(f: D => (Option[FreeK[F, Unit]], Option[(D, Δ) => Trigger[FreeK[F, Unit]]]))(implicit dom: Dom.Aux[D, U, Δ]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.observe[FreeK[F, ?], D, U, Δ](ref)(f))

  def selTrigger[L <: HList](sel: Sel[DRef, L])(f: (L) => Trigger[FreeK[F, Unit]]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.selTrigger[FreeK[F, ?], L](sel)(f))
}