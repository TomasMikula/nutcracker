package nutcracker

import scala.language.existentials

import PartialSolution._

import shapeless.HList
import nutcracker.util.Mapped

class RelTable private() {

  def findByRel[L <: HList](rel: Rel[L]): List[(Refs, Mapped.Aux[L, CellRef, Refs]) forSome { type Refs <: HList }] = ???

  def findByCell[D](ref: CellRef[D]): List[(Rel[L], Refs, Mapped.Aux[L, CellRef, Refs]) forSome { type L <: HList; type Refs <: HList }] = ???
}

object RelTable {
  val empty = new RelTable
}