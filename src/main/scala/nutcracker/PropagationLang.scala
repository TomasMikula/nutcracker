package nutcracker

import scala.language.higherKinds

import shapeless.{Sized, Nat, HList}

sealed trait PropagationLang[K[_], A]

object PropagationLang {

  case class Variable[K[_], A, D](d: D, dom: Domain[A, D]) extends PropagationLang[K, DomRef[A, D]]
  case class VarTrigger[K[_], D](ref: CellRef[D], f: D => Trigger[K]) extends PropagationLang[K, Unit]
  case class SelTrigger[K[_], L <: HList](sel: Sel[L], f: L => Trigger[K]) extends PropagationLang[K, Unit]
  case class Intersect[K[_], D](ref: CellRef[D], d: D) extends PropagationLang[K, Unit]
  case class IntersectVector[K[_], D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]) extends PropagationLang[K, Unit]
  case class Fetch[K[_], A, D](ref: DomRef[A, D]) extends PropagationLang[K, D]
  case class FetchVector[K[_], D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]) extends PropagationLang[K, Sized[Vector[D], N]]
  case class WhenResolved[K[_], A, D](ref: DomRef[A, D], f: A => K[Unit]) extends PropagationLang[K, Unit]

}