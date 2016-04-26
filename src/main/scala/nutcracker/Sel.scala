package nutcracker

import nutcracker.util.Mapped
import shapeless._
import shapeless.PolyDefns.~>

/**
  * Selection of cells.
  */
trait Sel[L <: HList] {
  type Refs <: HList
  def refs: Refs
  def mapped: Mapped.Aux[L, VRef, Refs]

  def cells: Seq[VRef[_]] = mapped.toList(refs)

  def fetch(f: VRef ~> Id): L = {
    mapped.extract(refs, f)
  }
}

object Sel {
  type Aux[L <: HList, R <: HList] = Sel[L] { type Refs = R }

  def apply[L <: HList, Refs0 <: HList](refs0: Refs0)(implicit m: Mapped.Aux[L, VRef, Refs0]): Sel.Aux[L, Refs0] = new Sel[L] {
    type Refs = Refs0
    def refs = refs0
    def mapped = m
  }

  def apply[D](ref: VRef[D]): Sel[D :: HNil] = apply(ref :: HNil)
  def apply[D1, D2](ref1: VRef[D1], ref2: VRef[D2]): Sel[D1 :: D2 :: HNil] = apply(ref1 :: ref2 :: HNil)
}