package nutcracker

import nutcracker.Pattern.Orientation
import nutcracker.util.{HList, HListPtr, Nat, Pointers, ValuedPointers}
import nutcracker.util.HList.{::, HNil, Length}
import nutcracker.util.Nat.{Succ, ToInt, _0}
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

  private[nutcracker] def head: RelChoice[V, _ <: HList]
  private[nutcracker] def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]]
}

private[nutcracker] sealed trait Pattern0[V <: HList, L <: HList] extends Pattern[V]

private[nutcracker] case class SingleRelPattern[V <: HList, L <: HList, N <: Nat](rel: RelChoice[V, L])(implicit l: Length.Aux[V, N], nToInt: ToInt[N]) extends Pattern0[V, L] {
  override def head = rel
  override def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]] = rel.matches(r) map { SingleRelPattern(_) }
  override val vertexCount = nToInt()
  override def vertexSet = rel.vertexSet
  override def relations = List(rel)
  override def emptyAssignment = Assignment[V].empty
}

private[nutcracker] case class ComposedPattern[V <: HList, L <: HList](
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

  abstract class Builder[V[_ <: HList] <: HList, Ptrs[_ <: HList, _ <: HList], N[_ <: Nat] <: Nat] { self =>
    type Next[B, T <: HList] = V[B :: T]
    type NextPtrs[B, T <: HList, PT <: HList] = Ptrs[B :: T, HListPtr.Aux[V[B :: T], N[_0], B] :: PT]
    type NextN[M <: Nat] = N[Succ[M]]

    protected def pointers[T <: HList, PT <: HList](pt: PT): Ptrs[T, PT]

    protected def ptr[T <: HList, I <: Nat, X](implicit ptr: HListPtr.Aux[T, I, X]): HListPtr.Aux[V[T], N[I], X]

    protected def length[T <: HList](implicit lt: Length[T]): HList.Length.Aux[V[T], N[lt.Out]]

    protected def nToInt[M <: Nat](implicit m: ToInt[M]): ToInt[N[M]]

    def apply[B]: Builder[({ type Out[T <: HList] = Next[B, T] })#Out, ({ type Out[T <: HList, PT <: HList] = NextPtrs[B, T, PT] })#Out, NextN] =
      new Builder[({ type Out[T <: HList] = Next[B, T] })#Out, ({ type Out[T <: HList, PT <: HList] = NextPtrs[B, T, PT] })#Out, NextN] {
        override def length[T <: HList](implicit lt: Length[T]): Length.Aux[V[B :: T], N[Succ[lt.Out]]] =
          self.length[B :: T](Length.consLength[B, T, lt.Out](lt))

        override def nToInt[M <: Nat](implicit m: ToInt[M]): ToInt[N[Succ[M]]] =
          self.nToInt[Succ[M]]

        override def ptr[T <: HList, I <: Nat, X](implicit ptr: HListPtr.Aux[T, I, X]): HListPtr.Aux[V[B :: T], N[Succ[I]], X] =
          self.ptr[B :: T, Succ[I], X]

        override def pointers[T <: HList, PT <: HList](pt: PT): Ptrs[B :: T, HListPtr.Aux[V[B :: T], N[_0], B] :: PT] =
          self.pointers[B :: T, HListPtr.Aux[V[B :: T], N[_0], B] :: PT](self.ptr[B :: T, _0, B] :: pt)
      }

    def build(
      f: Ptrs[HNil, HNil] => NonEmptyList[RelChoice[V[HNil], _ <: HList]],
    ): Pattern[V[HNil]] =
      Pattern.build[V[HNil], N[_0]](f(pointers[HNil, HNil](HNil)))(length[HNil], nToInt[_0]): Pattern[V[HNil]]
  }

  def apply[V <: HList](implicit ptrs: Pointers[V]): PatternBuilder[V, ptrs.Out] = PatternBuilder(ptrs.get)

  /** An alternative to [[apply]] that does not require implicit [[Pointers]], and thus is more robust. */
  def on[A]: Builder[({ type Out[T <: HList] = A :: T })#Out, ({ type Out[T <: HList, PT <: HList] = HListPtr.Aux[A :: T, _0, A] :: PT })#Out, Succ] =
    new Builder[({ type Out[T <: HList] = A :: T })#Out, ({ type Out[T <: HList, PT <: HList] = HListPtr.Aux[A :: T, _0, A] :: PT })#Out, Succ] {
      override def pointers[T <: HList, PT <: HList](pt: PT): HListPtr.Aux[A :: T, _0, A] :: PT =
        HListPtr.hlistAtZero[A, T] :: pt

      override def length[T <: HList](implicit lt: Length[T]): Length.Aux[A :: T, Succ[lt.Out]] =
        Length.consLength[A, T, lt.Out](lt)

      override def nToInt[M <: Nat](implicit m: ToInt[M]): ToInt[Succ[M]] =
        ToInt.succToInt[M]

      override def ptr[T <: HList, I <: Nat, X](implicit ptr: HListPtr.Aux[T, I, X]): HListPtr.Aux[A :: T, Succ[I], X] =
        HListPtr.hlistAtN(ptr)
    }

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

case class OrientedPattern[V <: HList, L <: HList] private[nutcracker] (pattern: Pattern[V], rel: Rel[L]) {

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

case class PartiallyAssignedPattern[V <: HList](pattern: Pattern[V], assignment: Assignment[V]) {
  def orient[L <: HList](rel: Rel[L]): PartiallyAssignedOrientedPattern[V, L] =
    PartiallyAssignedOrientedPattern(pattern.orient(rel), assignment)
}

case class PartiallyAssignedOrientedPattern[V <: HList, L <: HList](pattern: OrientedPattern[V, L], assignment: Assignment[V]) {
  def unorient: PartiallyAssignedPattern[V] = PartiallyAssignedPattern(pattern.pattern, assignment)
}