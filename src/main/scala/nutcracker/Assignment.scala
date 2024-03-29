package nutcracker

import nutcracker.util.{Choose, HList, HListPtr, Mapped, Nat}
import nutcracker.util.HList.{HNil, Length, ::}
import nutcracker.util.Nat.ToInt
import scala.annotation.tailrec
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.traverse._

case class Assignment[L <: HList] private (values: Vector[Option[_]]) extends AnyVal {

  def isEmpty: Boolean = values all { _.isEmpty }
  def getIfComplete: Option[L] = values.sequence map { _.foldRight[HList](HNil)(_ :: _).asInstanceOf[L] }
  def isExtensionOf(that: Assignment[L]): Boolean = {
    @tailrec def from(i: Int): Boolean =
      if(i >= values.size) true
      else {
        val a = this.values(i) // linter:ignore UndesirableTypeInference
        val b = that.values(i) // linter:ignore UndesirableTypeInference
        (b.isEmpty || b == a) && from(i+1)
      }
    from(0)
  }

  def get[C <: HList](ch: Choose[L, C]): Assignment[C] = Assignment(ch.vertices.map(values(_)).toVector)
  def set[C <: HList](ch: Choose[L, C])(c: C): Assignment[L] = set1(ch.vertices, c)
  def extend[C <: HList](ch: Choose[L, C])(c : C): Option[Assignment[L]] =
    if(matchesAt(ch)(c)) Some(set1(ch.vertices, c))
    else None

  def get(ptr: HListPtr[L, _]): Option[ptr.Out] = values(ptr.index).asInstanceOf[Option[ptr.Out]]
  def set(ptr: HListPtr[L, _])(a: ptr.Out): Assignment[L] = set0(ptr.index, a)

  def matches(l: L): Boolean = matches0(l, 0)
  def matchesAt[C <: HList](ch: Choose[L, C])(c: C): Boolean = matches0(ch.vertices, c)

  private def matches0[M <: HList](m: M, from: Int): Boolean = m match {
    case HNil => assert(from == values.size); true
    case h :: t => values(from).map(_ == h).getOrElse(true) && matches0(t, from + 1)
  }

  private def matches0[C <: HList](vertices: List[Int], c: C): Boolean = c match {
    case HNil => assert(vertices.isEmpty); true
    case h :: t => values(vertices.head).map(_ == h).getOrElse(true) && matches0(vertices.tail, t)
  }

  private def set0(i: Int, a: Any): Assignment[L] = Assignment(values.updated(i, Some(a)))
  @tailrec private def set1(is: List[Int], vs: HList): Assignment[L] = vs match {
    case HNil => assert(is.isEmpty); this
    case h :: t => set0(is.head, h).set1(is.tail, t)
  }
}

object Assignment {
  def apply[L <: HList]: AssignmentBuilder[L] = AssignmentBuilder()

  case class AssignmentBuilder[L <: HList] private[Assignment] () {
    def empty[N <: Nat](implicit l: Length.Aux[L, N], nToInt: ToInt[N]): Assignment[L] =
      Assignment(Vector.fill(nToInt())(Option.empty))

    def from[OL <: HList](ol: OL)(implicit m: Mapped.Aux[L, Option, OL]): Assignment[L] =
      Assignment(m.toList(ol).map(_.value).toVector)
  }
}