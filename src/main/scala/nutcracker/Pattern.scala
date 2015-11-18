package nutcracker

import nutcracker.util.Mapped

import shapeless.{:: => _, _}
import shapeless.poly.{~>, ~>>}
import shapeless.ops.hlist.At

sealed trait Pattern[V <: HList] {
  import Pattern._

  def rel[L <: HList, Ptrs <: HList](
      rel: Rel[L],
      ptrs: Ptrs)(implicit
      ev: Mapped.Aux[L, Ptr[V, ?], Ptrs]): Pattern[V] = ComposedPattern(this, rel, ptrs, ev)
}

object Pattern {
  type Ptr[V <: HList, A] = nutcracker.util.Ptr.Aux[V, _, A]

  private case class VertexIndex(value: Int) extends AnyVal
  private case class ComponentId private(value: Int) extends AnyVal {
    def next = ComponentId(value + 1)
  }
  private object ComponentId {
    val Zero = ComponentId(0)
  }

  private case class EmptyPattern[V <: HList]() extends Pattern[V]

  private case class ComposedPattern[V <: HList, L <: HList, Ptrs <: HList](
      base: Pattern[V],
      rel: Rel[L],
      ptrs: Ptrs,
      m: Mapped.Aux[L, Ptr[V, ?], Ptrs]) extends Pattern[V] {

    def indices: List[VertexIndex] = m.toList(ptrs, ptrToVIdx[V])
  }


  def isConnected(p: Pattern[_]): Boolean = {
    val componentAssignment = getComponentAssignment(p, ComponentId.Zero)
    val componentCount = componentAssignment.values.groupBy(i => i).size
    componentCount <= 1
  }

  private def ptrToVIdx[V <: HList]: Ptr[V, ?] ~>> VertexIndex = new ~>>[Ptr[V, ?], VertexIndex] {
    def apply[A](ptr: Ptr[V, A]): VertexIndex = VertexIndex(ptr.index)
  }

  private def getComponentAssignment(p: Pattern[_], c: ComponentId): Map[VertexIndex, ComponentId] = p match {
    case EmptyPattern() => Map()
    case cp @ ComposedPattern(base, _, _, _) =>
      val assignment0 = getComponentAssignment(base, c.next)
      val indices = cp.indices
      val components = (indices map { assignment0.get(_) } collect { case Some(comp) => comp }).toSet.toList
      components match {
        case Nil => assignment0 ++ (indices map { (_, c) })
        case c0 :: cs => (assignment0 mapValues { c1 => if(cs contains c1) c0 else c1 }) ++ (indices map { (_, c0) })
      }
  }
}