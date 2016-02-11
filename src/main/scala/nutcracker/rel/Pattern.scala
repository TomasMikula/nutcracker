package nutcracker.rel

import nutcracker.rel.Pattern.Orientation
import nutcracker.rel.RelDB.PartiallyAssignedPattern
import nutcracker.util.{ValuedPointers, Ptr, Mapped, Pointers}
import shapeless.ops.hlist.Length
import shapeless.ops.nat.ToInt
import shapeless.{:: => _, _}

import scala.annotation.tailrec
import scalaz.{ICons, IList, INil, NonEmptyList}

sealed trait Pattern[V <: HList] {

  def vertexCount: Int
  def vertexSet: Set[Int]
  def relations: List[RelChoice[V, _ <: HList]]
  def emptyAssignment: Assignment[V]

  def isCovered: Boolean = vertexSet == (0 until vertexCount).toSet

  def +[L <: HList](rel: RelChoice[V, L]): Pattern[V] = ComposedPattern(rel, this)

  def orient[L <: HList](r: Rel[L]): OrientedPattern[V, L] = OrientedPattern(this, r)

  final override def equals(other: Any): Boolean = other match {
    case that: Pattern[_] => that.relations.toSet == this.relations.toSet
    case _ => false
  }
  final override def hashCode: Int = relations.toSet.hashCode()

  private[rel] def head: RelChoice[V, _ <: HList]
  private[rel] def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]]
}

private[rel] sealed trait Pattern0[V <: HList, L <: HList] extends Pattern[V]

private[rel] case class SingleRelPattern[V <: HList, L <: HList, N <: Nat](rel: RelChoice[V, L])(implicit l: Length.Aux[V, N], nToInt: ToInt[N]) extends Pattern0[V, L] {
  override def head = rel
  override def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]] = rel.matches(r) map { SingleRelPattern(_) }
  override val vertexCount = nToInt()
  override def vertexSet = rel.vertexSet
  override def relations = List(rel)
  override def emptyAssignment = Assignment[V].empty
}

private[rel] case class ComposedPattern[V <: HList, L <: HList](
  rel: RelChoice[V, L],
  base: Pattern[V]
) extends Pattern0[V, L] {

  require((base.vertexSet intersect rel.vertexSet).nonEmpty, "Relation does not connect to the pattern built so far.")

  override def head = rel
  override def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]] = rel.matches(r) map { ComposedPattern(_, base) }
  override val vertexCount = base.vertexCount
  override lazy val vertexSet: Set[Int] = base.vertexSet union rel.vertexSet
  override def relations = rel :: base.relations
  override lazy val emptyAssignment = base.emptyAssignment
}

object Pattern {

  type Orientation[V <: HList, L <: HList] = (RelChoice[V, L], List[RelChoice[V, _ <: HList]])

  case class PatternBuilder[V <: HList, Ptrs <: HList](ptrs: Ptrs) {
    def build[N <: Nat](f: Ptrs => NonEmptyList[RelChoice[V, _ <: HList]])(implicit l: Length.Aux[V, N], nToInt: ToInt[N]): Pattern[V] = Pattern.build(f(ptrs))
    def where[PA <: HList, N <: Nat](f: Ptrs => PA)(implicit
      vp: ValuedPointers[V, PA],
      l: Length.Aux[V, N],
      nToInt: ToInt[N]
    ): PartiallyAssignedPatternBuilder[V, Ptrs] = {
      val (ch, c) = vp(f(ptrs))
      val asg = Assignment[V].empty.set(ch)(c)
      PartiallyAssignedPatternBuilder(ptrs, asg)
    }
  }

  case class PartiallyAssignedPatternBuilder[V <: HList, Ptrs <: HList](ptrs: Ptrs, asg: Assignment[V]) {
    def build[N <: Nat](f: Ptrs => NonEmptyList[RelChoice[V, _ <: HList]])(implicit l: Length.Aux[V, N], nToInt: ToInt[N]): PartiallyAssignedPattern[V] =
      PartiallyAssignedPattern(Pattern.build(f(ptrs)), asg)
  }

  def apply[V <: HList](implicit ptrs: Pointers[V]): PatternBuilder[V, ptrs.Out] = PatternBuilder(ptrs.get)

  private case class ComponentId private(value: Int) extends AnyVal {
    def next = ComponentId(value + 1)
  }
  private object ComponentId {
    val Zero = ComponentId(0)
  }


  private def build[V <: HList, N <: Nat](rels: NonEmptyList[RelChoice[V, _ <: HList]])(implicit l: Length.Aux[V, N], nToInt: ToInt[N]): Pattern[V] = {
    build0(rels.tail, SingleRelPattern(rels.head))
  }

  @tailrec
  private def build0[V <: HList](rels: IList[RelChoice[V, _ <: HList]], acc: Pattern[V]): Pattern[V] = {
    rels match {
      case INil() => acc
      case ICons(h, t) => build0(t, ComposedPattern(h, acc))
    }
  }

}

case class OrientedPattern[V <: HList, L <: HList] private[rel] (pattern: Pattern[V], rel: Rel[L]) {

  /**
    * For each occurrence of relation `rel` in `pattern`, contains
    * _some_ rearrangement of `pattern`'s relations such that `rel` is the
    * first one and every subsequent relation shares at least one common
    * vertex with at least one of the previous relations. In other words,
    * any prefix of an orientation forms a connected component.
    *
    * The size of the returned collection is the same as the number of
    * occurrences of `rel` in this pattern.
    */
  lazy val orientations: List[Orientation[V, L]] = OrientedPattern.orient(pattern, rel)
}

object OrientedPattern {

  private def orient[V <: HList, L <: HList](pat: Pattern[V], rel: Rel[L]): List[Orientation[V, L]] =
    orient0(pat, rel, Nil, Nil)

  @tailrec
  private def orient0[V <: HList, L <: HList](
    pat: Pattern[V],
    point: Rel[L],
    tail: List[RelChoice[V, _ <: HList]],
    solutionAcc: List[Orientation[V, L]]
  ): List[Orientation[V, L]] = {
    val acc1: List[Orientation[V, L]] = pat.matchHead(point).map({ orient1(_, tail) }) match {
      case Some(sol) => sol :: solutionAcc
      case None => solutionAcc
    }
    pat match {
      case SingleRelPattern(_) => acc1
      case ComposedPattern(r, pat1) => orient0(pat1, point, r::tail, acc1)
    }
  }

  private def orient1[V <: HList, L <: HList](pat: Pattern0[V, L], tail: List[RelChoice[V, _ <: HList]]): Orientation[V, L] = pat match {
    case SingleRelPattern(r) => (r, tail)
    case ComposedPattern(r, base) => orient1(base, r, Nil, r.vertexSet, tail)
  }

  @tailrec
  private def orient1[V <: HList, L <: HList](
    pat: Pattern[V],
    head: RelChoice[V, L],
    revNext: List[RelChoice[V, _ <: HList]],
    coveredVertices: Set[Int],
    tail: List[RelChoice[V, _ <: HList]]
  ): Orientation[V, L] = {

    assert((coveredVertices intersect pat.vertexSet).nonEmpty)

    val (revNext1, tail1, coveredVertices1) =
      if((coveredVertices intersect pat.head.vertexSet).nonEmpty) (pat.head :: revNext, tail, coveredVertices union pat.head.vertexSet)
      else (revNext, pat.head :: tail, coveredVertices)

    pat match {
      case SingleRelPattern(_) => (head, revNext1 reverse_::: tail1)
      case ComposedPattern(_, base) => orient1(base, head, revNext1, coveredVertices1, tail1)
    }
  }

}