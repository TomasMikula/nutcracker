package nutcracker.rel

import nutcracker.util.{Ptr, Choose}

import scala.annotation.tailrec
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.traverse._
import shapeless.ops.nat.ToInt
import shapeless.{HNil, ::, Nat, HList}
import shapeless.ops.hlist.Length

case class Assignment[L <: HList] private (values: Vector[Option[_]]) extends AnyVal {

  def getIfComplete: Option[L] = values.sequence map { _.foldRight[HList](HNil)(_ :: _).asInstanceOf[L] }

  def get[C <: HList](ch: Choose[L, C]): Assignment[C] = Assignment(ch.vertices.map(values(_)).toVector)
  def set[C <: HList](ch: Choose[L, C])(c: C): Assignment[L] = set1(ch.vertices, c)

  def get(ptr: Ptr[L, _]): Option[ptr.Out] = values(ptr.index).asInstanceOf[Option[ptr.Out]]
  def set[A](ptr: Ptr.Aux[L, _, A])(a: A): Assignment[L] = set0(ptr.index, a)

  def matches(l: L): Boolean = matches0(l, 0)

  private def matches0[M <: HList](m: M, from: Int): Boolean = m match {
    case HNil => assert(from == values.size); true
    case h :: t =>
      if(!values(from).map(_ == h).getOrElse(false)) false else this.matches0(t, from + 1)
  }

  private def set0(i: Int, a: Any): Assignment[L] = Assignment(values.updated(i, Some(a)))
  @tailrec private def set1(is: List[Int], vs: HList): Assignment[L] = vs match {
    case HNil => assert(is.isEmpty); this
    case h :: t => set0(is.head, h).set1(is.tail, t)
  }
}

object Assignment {
  def apply[L <: HList](implicit l: Length[L]): AssignmentBuilder[L, l.Out] = AssignmentBuilder()

  case class AssignmentBuilder[L <: HList, N <: Nat] private[rel] () {
    def empty(implicit nToInt: ToInt[N]): Assignment[L] = Assignment(Vector.fill(nToInt())(Option.empty))
  }
}