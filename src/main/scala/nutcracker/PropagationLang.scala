package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK, StateInterpreter}
import shapeless.HList

import scalaz.{Functor, ~>}

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  // constructors (the instruction set of a free program)
  case class Cell[K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, DRef.Aux[D, U, Δ]]
  case class Update[K[_], D, U, Δ](ref: DRef.Aux[D, U, Δ], u: U, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]
  case class DomTrigger[K[_], D, U, Δ](ref: DRef.Aux[D, U, Δ], f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K[Unit]]])) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K[Unit]]) extends PropagationLang[K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def cell[K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[K, DRef.Aux[D, dom.Update, dom.Delta]] =
    Cell(d, dom)
  def update[K[_], D](ref: DRef[D])(u: ref.Update)(implicit dom: Dom.Aux[D, ref.Update, ref.Delta]): PropagationLang[K, Unit] =
    Update[K, D, ref.Update, ref.Delta](ref, u, dom)
  def domTrigger[K[_], D](ref: DRef[D])(f: D => (Option[K[Unit]], Option[(D, ref.Delta) => Trigger[K[Unit]]])): PropagationLang[K, Unit] =
    DomTrigger[K, D, ref.Update, ref.Delta](ref, f)
  def selTrigger[K[_], L <: HList](sel: Sel[L])(f: L => Trigger[K[Unit]]): PropagationLang[K, Unit] =
    SelTrigger(sel, f)


  implicit def functorKInstance: FunctorKA[PropagationLang] = new FunctorKA[PropagationLang] {

    private def ftr[A, B, K[_], L[_]](f: (A, B) => Trigger[K[Unit]], tr: K ~> L)(implicit ft: Functor[Trigger]): (A, B) => Trigger[L[Unit]] =
      (a, b) => ft.map(f(a, b))(tr[Unit] _)

    def transform[K[_], L[_], A](pk: PropagationLang[K, A])(tr: K ~> L): PropagationLang[L, A] = pk match {

      // the interesting cases
      case DomTrigger(ref, f) => domTrigger(ref){ d =>
        val (now, onChange) = f(d)
        (now map (tr(_)), onChange map (action => ftr(action, tr)))
      }
      case SelTrigger(sel, f)   => selTrigger(sel){ l => Functor[Trigger].map(f(l))(tr[Unit] _) }

      // the boring cases
      case Cell(d, dom)        => Cell(d, dom)
      case Update(ref, u, dom) => Update(ref, u, dom)
    }
  }

  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[PropagationLang, F]): Propagation[FreeK[F, ?]] =
    new FreePropagation[F]

  implicit def interpreter: StateInterpreter[PropagationLang, PropagationStore] = PropagationStore.interpreter
}


private[nutcracker] class FreePropagation[F[_[_], _]](implicit inj: InjectK[PropagationLang, F]) extends Propagation[FreeK[F, ?]] {

  def cell[D](d: D)(implicit dom: Dom[D]): FreeK[F, DRef.Aux[D, dom.Update, dom.Delta]] =
    FreeK.injLiftF(PropagationLang.cell[FreeK[F, ?], D](d))

  def update[D](ref: DRef[D])(u: ref.Update)(implicit dom: Dom.Aux[D, ref.Update, ref.Delta]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.update[FreeK[F, ?], D](ref)(u))

  def domTrigger[D](ref: DRef[D])(f: (D) => (Option[FreeK[F, Unit]], Option[(D, ref.Delta) => Trigger[FreeK[F, Unit]]])): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.domTrigger[FreeK[F, ?], D](ref)(f))

  def selTrigger[L <: HList](sel: Sel[L])(f: (L) => Trigger[FreeK[F, Unit]]): FreeK[F, Unit] =
    FreeK.injLiftF(PropagationLang.selTrigger[FreeK[F, ?], L](sel)(f))
}