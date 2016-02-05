package nutcracker.rel

import algebra.Order
import nutcracker.util.Mapped
import shapeless.HList

sealed trait RelTable[L <: HList] {
  def insert(row: L): Option[RelTable[L]]
  def query(q: Assignment[L]): List[L]
  def size: Int
  def rows: Vector[L]
}

object RelTable {
  def apply[L <: HList](implicit m: Mapped[L, Order]): RelTableBuilder[L, m.Out] = RelTableBuilder()(m)

  case class RelTableBuilder[L <: HList, OS <: HList] private[RelTable] ()(implicit m: Mapped.Aux[L, Order, OS]) {
    def empty(implicit orders: OS): RelTable[L] = new RelTable0[L, OS](Vector.empty)
  }
}

// XXX This implementation is really inefficient. Need to use indices.
private[rel] case class RelTable0[L <: HList, OS <: HList](rows: Vector[L])(implicit m: Mapped.Aux[L, Order, OS], orders: OS) extends RelTable[L] {

  def size = rows.size

  def insert(row: L): Option[RelTable[L]] =
    if(rows.contains(row)) None
    else Some(RelTable0(rows :+ row))

  def query(q: Assignment[L]): List[L] =
    rows.foldRight[List[L]](Nil)((row, acc) => if(q.matches(row)) row :: acc else acc)
}
