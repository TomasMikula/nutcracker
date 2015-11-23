package nutcracker

import nutcracker.util.Mapped
import shapeless._

/**
  * Selection of cells.
  */
trait Sel[L <: HList] {
  type Refs <: HList
  def refs: Refs
  def cells: Seq[CellRef[_]]
}

object Sel {
  type Aux[L <: HList, R <: HList] = Sel[L] { type Refs = R }

  def apply[L <: HList, Refs0 <: HList](refs0: Refs0)(implicit m: Mapped.Aux[L, CellRef, Refs0]): Sel.Aux[L, Refs0] = new Sel[L] {
    type Refs = Refs0
    def refs = refs0
    def cells: Seq[CellRef[_]] = m.toList(refs0)
  }

  def apply[D](ref: CellRef[D]): Sel[D :: HNil] = apply(ref :: HNil)
  def apply[D1, D2](ref1: CellRef[D1], ref2: CellRef[D2]): Sel[D1 :: D2 :: HNil] = apply(ref1 :: ref2 :: HNil)
}