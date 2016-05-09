package nutcracker

import scala.language.higherKinds

import nutcracker.util.{FreeK, FunctorK, FunctorKA, InjectK, StateInterpreterT}
import shapeless.{::, HList, HNil, Nat, Sized}

import scalaz.{Traverse, ~>}
import scalaz.std.vector._

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  private type FP[A] = FreeK[PropagationLang, A]

  // constructors (the instruction set of a free program)
  case class Cell[K[_], D, U, Δ](d: D, dom: Dom[D, U, Δ]) extends PropagationLang[K, DRef[D, U, Δ]]
  case class Update[K[_], D, U, Δ](ref: DRef[D, U, Δ], u: U, dom: Dom[D, U, Δ]) extends PropagationLang[K, Unit]
  case class Fetch[K[_], D](ref: VRef[D]) extends PropagationLang[K, D]
  case class FetchVector[K[_], D, N <: Nat](refs: Sized[Vector[VRef[D]], N]) extends PropagationLang[K, Sized[Vector[D], N]]
  case class DomTrigger[K[_], D, U, Δ](ref: DRef[D, U, Δ], f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K]])) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K]) extends PropagationLang[K, Unit]

  // constructors returning less specific types, and curried to help with type inference
  def cell[K[_], D, U, Δ](d: D)(implicit dom: Dom[D, U, Δ]): PropagationLang[K, DRef[D, U, Δ]] = Cell(d, dom)
  def update[K[_], D, U, Δ](ref: DRef[D, U, Δ])(u: U)(implicit dom: Dom[D, U, Δ]): PropagationLang[K, Unit] = Update(ref, u, dom)
  def fetch[K[_], D](ref: VRef[D]): PropagationLang[K, D] = Fetch(ref)
  def fetchVector[K[_], D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): PropagationLang[K, Sized[Vector[D], N]] = FetchVector(refs)
  def domTrigger[K[_], D, U, Δ](ref: DRef[D, U, Δ])(f: D => (Option[K[Unit]], Option[(D, Δ) => Trigger[K]])): PropagationLang[K, Unit] = DomTrigger(ref, f)
  def selTrigger[K[_], L <: HList](sel: Sel[L])(f: L => Trigger[K]): PropagationLang[K, Unit] = SelTrigger(sel, f)

  // constructors lifted to free programs
  def cellF[D, U, Δ](d: D)(implicit dom: Dom[D, U, Δ]): FP[DRef[D, U, Δ]] =
    FreeK.suspend(cell[FP, D, U, Δ](d))
  def updateF[D, U, Δ](ref: DRef[D, U, Δ])(u: U)(implicit dom: Dom[D, U, Δ]): FP[Unit] =
    FreeK.suspend(update[FP, D, U, Δ](ref)(u))
  def fetchF[D](ref: VRef[D]): FP[D] =
    FreeK.suspend(fetch[FP, D](ref))
  def fetchVectorF[D, N <: Nat](refs: Sized[Vector[VRef[D]], N]): FP[Sized[Vector[D], N]] =
    FreeK.suspend(fetchVector[FP, D, N](refs))
  def domTriggerF[F[_[_], _], D, U, Δ](ref: DRef[D, U, Δ])(f: D => (Option[FreeK[F, Unit]], Option[(D, Δ) => Trigger[FreeK[F, ?]]]))(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(domTrigger[FreeK[F, ?], D, U, Δ](ref)(f))
  def selTriggerF[F[_[_], _], L <: HList](sel: Sel[L])(f: L => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    FreeK.lift(selTrigger[FreeK[F, ?], L](sel)(f))


  // convenience API

  def cellsF[D, U, Δ](d: D, n: Int)(implicit dom: Dom[D, U, Δ]): FP[Vector[DRef[D, U, Δ]]] =
    Traverse[Vector].sequenceU(Vector.fill(n)(cellF(d)))

  def valTriggerF[F[_[_], _], D](ref: DRef[D, _, _])(f: D => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    domTriggerF(ref)(d => f(d) match {
      case FireReload(k) => (Some(k), Some((d, δ) => f(d)))
      case Fire(k) => (Some(k), None)
      case Sleep() => (None, Some((d, δ) => f(d)))
      case Discard() => (None, None)
    })

  def whenRefinedF[F[_[_], _], D](ref: DRef[D, _, _])(f: D => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], dom: Dom[D, _, _]): FreeK[F, Unit] =
    valTriggerF[F, D](ref)(d => dom.assess(d) match {
      case Dom.Refined => Fire[FreeK[F, ?]](f(d))
      case _ => Sleep[FreeK[F, ?]]()
    })

  def whenResolvedF[F[_[_], _], A, D](ref: DRef[D, _, _])(f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): FreeK[F, Unit] =
    valTriggerF[F, D](ref)(d => ee.extract(d) match {
      case Some(a) => Fire[FreeK[F, ?]](f(a))
      case None => Sleep[FreeK[F, ?]]()
    })

  def selTrigger2F[F[_[_], _], D1, D2](ref1: VRef[D1], ref2: VRef[D2])(f: (D1, D2) => Trigger[FreeK[F, ?]])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] =
    selTriggerF[F, D1 :: D2 :: HNil](Sel(ref1, ref2))(l => f(l.head, l.tail.head))


  implicit def functorKInstance: FunctorKA[PropagationLang] = new FunctorKA[PropagationLang] {

    private def ftr[A, B, K[_], L[_]](f: (A, B) => Trigger[K], tr: K ~> L)(implicit fkt: FunctorK[Trigger]): (A, B) => Trigger[L] =
      (a, b) => fkt.transform(f(a, b))(tr)

    def transform[K[_], L[_], A](pk: PropagationLang[K, A])(tr: K ~> L): PropagationLang[L, A] = pk match {

      // the interesting cases
      case DomTrigger(ref, f) => domTrigger(ref){ d =>
        val (now, onChange) = f(d)
        (now map (tr(_)), onChange map (action => ftr(action, tr)))
      }
      case SelTrigger(sel, f)   => selTrigger(sel){ l => FunctorK[Trigger].transform(f(l))(tr) }

      // the boring cases
      case Cell(d, dom)        => Cell(d, dom)
      case Update(ref, u, dom) => Update(ref, u, dom)
      case Fetch(ref)          => Fetch(ref)
      case FetchVector(refs)   => FetchVector(refs)
    }
  }

  implicit def interpreter: StateInterpreterT.StateInterpreter.Aux[PropagationLang, PropagationStore] = PropagationStore.interpreter
}