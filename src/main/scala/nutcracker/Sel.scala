package nutcracker

import scala.language.higherKinds

import nutcracker.util.Mapped
import shapeless._
import shapeless.PolyDefns.~>

/**
  * Selection of cells.
  */
trait Sel[Ref[_], L <: HList] {
  type Refs <: HList
  def refs: Refs
  def mapped: Mapped.Aux[L, Ref, Refs]

  def cells: Seq[Ref[_]] = mapped.toList(refs)

  def fetch(f: Ref ~> Id): L = {
    mapped.extract(refs, f)
  }
}

object Sel {
  type Aux[Ref[_], L <: HList, R <: HList] = Sel[Ref, L] { type Refs = R }

  def apply[Ref[_], L <: HList, Refs0 <: HList](refs0: Refs0)(implicit m: Mapped.Aux[L, Ref, Refs0]): Sel.Aux[Ref, L, Refs0] = new Sel[Ref, L] {
    type Refs = Refs0
    def refs = refs0
    def mapped = m
  }

  def apply[Ref[_], D](ref: Ref[D]): Sel[Ref, D :: HNil] = apply(ref :: HNil)
  def apply[Ref[_], D1, D2](ref1: Ref[D1], ref2: Ref[D2]): Sel[Ref, D1 :: D2 :: HNil] = apply(ref1 :: ref2 :: HNil)
}