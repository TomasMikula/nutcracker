package nutcracker

import scala.language.higherKinds
import nutcracker.util.{FreeK, FunctorKA, InjectK, StateInterpreter}
import shapeless.{::, HList, HNil, Nat, Sized}

import scalaz.{Functor, Traverse, ~>}
import scalaz.std.vector._

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  // constructors (the instruction set of a free program)
  case class Cell[K[_], D, U, Δ](d: D, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, DRef.Aux[D, U, Δ]]
  case class Update[K[_], D, U, Δ](ref: DRef.Aux[D, U, Δ], u: U, dom: Dom.Aux[D, U, Δ]) extends PropagationLang[K, Unit]
  case class Fetch[K[_], D](ref: DRef[D]) extends PropagationLang[K, D]
  case class FetchVector[K[_], D, N <: Nat](refs: Sized[Vector[DRef[D]], N]) extends PropagationLang[K, Sized[Vector[D], N]]
  case class DomTrigger[K[_], D, U, Δ](ref: DRef.Aux[D, U, Δ], f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K[Unit]]])) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K[Unit]]) extends PropagationLang[K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def cell[K[_], D](d: D)(implicit dom: Dom[D]): PropagationLang[K, DRef.Aux[D, dom.Update, dom.Delta]] =
    Cell(d, dom)
  def update[K[_], D](ref: DRef[D])(u: ref.Update)(implicit dom: Dom.Aux[D, ref.Update, ref.Delta]): PropagationLang[K, Unit] =
    Update[K, D, ref.Update, ref.Delta](ref, u, dom)
  def fetch[K[_], D](ref: DRef[D]): PropagationLang[K, D] =
    Fetch(ref)
  def fetchVector[K[_], D, N <: Nat](refs: Sized[Vector[DRef[D]], N]): PropagationLang[K, Sized[Vector[D], N]] =
    FetchVector(refs)
  def domTrigger[K[_], D](ref: DRef[D])(f: D => (Option[K[Unit]], Option[(D, ref.Delta) => Trigger[K[Unit]]])): PropagationLang[K, Unit] =
    DomTrigger[K, D, ref.Update, ref.Delta](ref, f)
  def selTrigger[K[_], L <: HList](sel: Sel[L])(f: L => Trigger[K[Unit]]): PropagationLang[K, Unit] =
    SelTrigger(sel, f)

  // constructors lifted to free programs
  def cellF[D](d: D)(implicit dom: Dom[D]): FP[DRef.Aux[D, dom.Update, dom.Delta]] =
    FreeK.liftF(cell[FP, D](d))
  def updateF[D](ref: DRef[D])(u: ref.Update)(implicit dom: Dom.Aux[D, ref.Update, ref.Delta]): FP[Unit] =
    FreeK.liftF(update[FP, D](ref)(u))
  def fetchF[D](ref: DRef[D]): FP[D] =
    FreeK.liftF(fetch[FP, D](ref))
  def fetchVectorF[D, N <: Nat](refs: Sized[Vector[DRef[D]], N]): FP[Sized[Vector[D], N]] =
    FreeK.liftF(fetchVector[FP, D, N](refs))
  def domTriggerF[F[_[_], _], D](ref: DRef[D])(f: D => (Option[FreeK[F, Unit]], Option[(D, ref.Delta) => Trigger[FreeK[F, Unit]]]))(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF(domTrigger[FreeK[F, ?], D](ref)(f))
  def selTriggerF[F[_[_], _], L <: HList](sel: Sel[L])(f: L => Trigger[FreeK[F, Unit]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.injLiftF(selTrigger[FreeK[F, ?], L](sel)(f))


  // convenience API

  def cellsF[D](d: D, n: Int)(implicit dom: Dom[D]): FP[Vector[DRef.Aux[D, dom.Update, dom.Delta]]] =
    Traverse[Vector].sequenceU(Vector.fill(n)(cellF(d)))

  def valTriggerF[F[_[_], _], D](ref: DRef[D])(f: D => Trigger[FreeK[F, Unit]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    domTriggerF(ref)(d => f(d) match {
      case FireReload(k) => (Some(k), Some((d, δ) => f(d)))
      case Fire(k) => (Some(k), None)
      case Sleep() => (None, Some((d, δ) => f(d)))
      case Discard() => (None, None)
    })

  def whenRefinedF[F[_[_], _], D](ref: DRef[D])(f: D => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], dom: Dom[D]): FreeK[F, Unit] =
    valTriggerF[F, D](ref)(d => dom.assess(d) match {
      case Dom.Refined => Fire[FreeK[F, Unit]](f(d))
      case _ => Sleep[FreeK[F, Unit]]()
    })

  def selTrigger2F[F[_[_], _], D1, D2](ref1: DRef[D1], ref2: DRef[D2])(f: (D1, D2) => Trigger[FreeK[F, Unit]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    selTriggerF[F, D1 :: D2 :: HNil](Sel(ref1, ref2))(l => f(l.head, l.tail.head))


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
      case Fetch(ref)          => Fetch(ref)
      case FetchVector(refs)   => FetchVector(refs)
    }
  }

  implicit def interpreter: StateInterpreter[PropagationLang, PropagationStore] = PropagationStore.interpreter
}
