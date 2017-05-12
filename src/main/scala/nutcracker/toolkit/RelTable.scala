package nutcracker.toolkit

import nutcracker.Assignment
import nutcracker.util.{Lst, Mapped}
import scala.annotation.tailrec
import scalaz.Order
import shapeless.HList

private[toolkit] sealed trait RelTable[K[_], L <: HList] {
  def insert(row: L): Option[RelTable[K, L]]
  def query(q: Assignment[L]): List[L]
  def execWith(q: Assignment[L])(supply: RelToken[L] => K[Unit])(exec: L => K[Unit]): (Option[RelTable[K, L]], Option[K[Unit]])
  def supply(t: RelToken[L], row: L): (RelTable[K, L], Lst[K[Unit]])
  def size: Int
  def rows: Vector[L]
}

private[toolkit] object RelTable {
  def apply[K[_], L <: HList](implicit m: Mapped[L, Order]): RelTableBuilder[K, L, m.Out] = RelTableBuilder[K, L, m.Out]()(m)

  case class RelTableBuilder[K[_], L <: HList, OS <: HList] private[RelTable] ()(implicit m: Mapped.Aux[L, Order, OS]) {
    def empty(implicit orders: OS): RelTable[K, L] = new RelTable0[K, L, OS](Vector.empty, Vector.empty, 0L)
  }
}

// XXX This implementation is really inefficient. Need to use indices.
private[toolkit] case class RelTable0[K[_], L <: HList, OS <: HList](
  rows: Vector[L],
  initializing: Vector[(RelToken[L], Assignment[L], List[L => K[Unit]])],
  nextRelTokenId: Long
)(implicit m: Mapped.Aux[L, Order, OS], orders: OS) extends RelTable[K, L] {

  def size = rows.size

  def insert(row: L): Option[RelTable[K, L]] =
    if(rows.contains(row)) None
    else Some(RelTable0(rows :+ row, initializing, nextRelTokenId))

  def query(q: Assignment[L]): List[L] =
    if(q.isEmpty) rows.toList
    else rows.foldRight[List[L]](Nil)((row, acc) => if(q.matches(row)) row :: acc else acc)

  def execWith(q: Assignment[L])(supply: RelToken[L] => K[Unit])(exec: L => K[Unit]): (Option[RelTable[K, L]], Option[K[Unit]]) =
    query(q) match {
      case h :: _ => (None, Some(exec(h)))
      case Nil =>
        val i = initializing.indexWhere(_._2.isExtensionOf(q))
        if(i >= 0) {
          val (t, ass, ks) = initializing(i)
          val init1 = initializing.updated(i, (t, ass, exec :: ks))
          (Some(RelTable0(rows, init1, nextRelTokenId)), None)
        } else {
          val token = RelToken[L](nextRelTokenId)
          (Some(RelTable0(rows, initializing :+ ((token, q, List(exec))), nextRelTokenId + 1)), Some(supply(token)))
        }
    }

  def supply(t: RelToken[L], row: L): (RelTable[K, L], Lst[K[Unit]]) = {
    val i = binarySearch(t)
    assert(i >= 0)
    val (_, ass, ks) = initializing(i)
    assert(ass.matches(row))
    (
      RelTable0(rows :+ row, initializing.slice(0, i) ++ initializing.slice(i+1, initializing.size), nextRelTokenId),
      ks.foldLeft(Lst.empty[K[Unit]])((lst, f) => f(row) :: lst)
    )
  }

  private def binarySearch(t: RelToken[L]): Int =
    binarySearch(t, 0, initializing.size)

  @tailrec private def binarySearch(t: RelToken[L], lo: Int, hi: Int): Int =
    if(lo == hi) -1
    else {
      val i = (lo + hi) / 2
      val u = initializing(i)._1
      if(u.value < t.value) binarySearch(t, i+1, hi)
      else if(u.value > t.value) binarySearch(t, lo, i)
      else i
    }
}

private[toolkit] case class RelToken[L](value: Long) extends AnyVal