package nutcracker

import nutcracker.util.Pointers
import scala.annotation.tailrec
import scalaz.{IList, ICons, INil, NonEmptyList}
import shapeless.{:: => _, _}

sealed trait Pattern[V <: HList] {
  import Pattern._

  def vertexSet: Set[Int]
  def relations: List[RelChoice[V, _]]

  def +[L <: HList](rel: RelChoice[V, L]): Pattern[V] = ComposedPattern(rel, this)

  def orient[L <: HList](r: Rel[L]): Seq[(RelChoice[V, L], Seq[RelChoice[V, _]])] = Pattern.orient(this, r)

  private[nutcracker] def head: RelChoice[V, _]
  private[nutcracker] def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]]
}

object Pattern {

  private type VertexIndex = Int

  case class PatternBuilder[V <: HList, Ptrs <: HList](ptrs: Ptrs) {
    def build(f: Ptrs => NonEmptyList[RelChoice[V, _ <: HList]]): Pattern[V] = Pattern.build(f(ptrs))
  }

  def apply[V <: HList](implicit ptrs: Pointers[V]): PatternBuilder[V, ptrs.Out] = PatternBuilder(ptrs.get)

  private case class ComponentId private(value: Int) extends AnyVal {
    def next = ComponentId(value + 1)
  }
  private object ComponentId {
    val Zero = ComponentId(0)
  }

  private[Pattern] sealed trait Pattern0[V <: HList, L <: HList] extends Pattern[V]

  private case class SingleRelPattern[V <: HList, L <: HList](rel: RelChoice[V, L]) extends Pattern0[V, L] {
    override def head = rel
    override def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]] = rel.matches(r) map { SingleRelPattern(_) }
    override def vertexSet = rel.vertexSet
    override def relations = List(rel)
  }

  private case class ComposedPattern[V <: HList, L <: HList](
    rel: RelChoice[V, L],
    base: Pattern[V]
  ) extends Pattern0[V, L] {

    require((base.vertexSet intersect rel.vertexSet).nonEmpty, "Relation does not connect to the pattern built so far.")

    override def head = rel
    override def matchHead[K <: HList](r: Rel[K]): Option[Pattern0[V, K]] = rel.matches(r) map { ComposedPattern(_, base) }
    override lazy val vertexSet: Set[Int] = base.vertexSet union rel.vertexSet
    override def relations = rel :: base.relations
  }


  private def build[V <: HList](rels: NonEmptyList[RelChoice[V, _ <: HList]]): Pattern[V] = {
    build0(rels.tail, SingleRelPattern(rels.head))
  }

  @tailrec
  private def build0[V <: HList](rels: IList[RelChoice[V, _ <: HList]], acc: Pattern[V]): Pattern[V] = {
    rels match {
      case INil() => acc
      case ICons(h, t) => build0(t, ComposedPattern(h, acc))
    }
  }


  /**
    * For each occurrence of relation `rel` in pattern `pat`, returns
    * _some_ rearrangement of `pat`'s relations such that `rel` is the
    * first one and every subsequent relation shares at least one common
    * vertex with at least one of the previous relations.
    *
    * The size of the returned collection is the same as the number of
    * occurrences of `rel` in this pattern.
    */
  def orient[V <: HList, L <: HList](pat: Pattern[V], rel: Rel[L]): Seq[(RelChoice[V, L], Seq[RelChoice[V, _]])] =
    orient0(pat, rel, Nil, Nil)

  @tailrec
  private def orient0[V <: HList, L <: HList](
    pat: Pattern[V],
    point: Rel[L],
    tail: List[RelChoice[V, _]],
    solutionAcc: List[(RelChoice[V, L], Seq[RelChoice[V, _]])]
  ): List[(RelChoice[V, L], Seq[RelChoice[V, _]])] = {
    val acc1: List[(RelChoice[V, L], Seq[RelChoice[V, _]])] = pat.matchHead(point).map({ orient1(_, tail) }) match {
      case Some(sol) => sol :: solutionAcc
      case None => solutionAcc
    }
    pat match {
      case SingleRelPattern(_) => acc1
      case ComposedPattern(r, pat1) => orient0(pat1, point, r::tail, acc1)
    }
  }

  private def orient1[V <: HList, L <: HList](pat: Pattern0[V, L], tail: List[RelChoice[V, _]]): (RelChoice[V, L], Seq[RelChoice[V, _]]) = pat match {
    case SingleRelPattern(r) => (r, tail)
    case ComposedPattern(r, base) => orient1(base, r, Nil, r.vertexSet, tail)
  }

  @tailrec
  private def orient1[V <: HList, L <: HList](
    pat: Pattern[V],
    head: RelChoice[V, L],
    revNext: List[RelChoice[V, _]],
    coveredVertices: Set[VertexIndex],
    tail: List[RelChoice[V, _]]
  ): (RelChoice[V, L], Seq[RelChoice[V, _]]) = {

    assert((coveredVertices intersect pat.vertexSet).nonEmpty)

    val (revNext1, tail1, coveredVertices1) =
      if((coveredVertices intersect pat.head.vertexSet).nonEmpty) (pat.head :: revNext, tail, coveredVertices union pat.head.vertexSet)
      else (revNext, pat.head :: tail, coveredVertices)

    pat match {
      case SingleRelPattern(_) => (head, revNext1 reverse_::: tail1)
      case ComposedPattern(_, base) => orient1(base, head, revNext1, coveredVertices1, tail1)
    }
  }

  def isConnected(p: Pattern[_]): Boolean = {
    val componentAssignment = getComponentAssignment(p, ComponentId.Zero)
    val componentCount = componentAssignment.values.groupBy(i => i).size
    componentCount <= 1
  }

  private def getComponentAssignment(p: Pattern[_], c: ComponentId): Map[VertexIndex, ComponentId] = p match {
    case SingleRelPattern(rel) => (rel.vertexSet map { (_, c) }).toMap
    case ComposedPattern(rel, base) =>
      val assignment0 = getComponentAssignment(base, c.next)
      val indices = rel.vertexSet
      val components = (indices map { assignment0.get(_) } collect { case Some(comp) => comp }).toList
      components match {
        case Nil => assignment0 ++ (indices map { (_, c) })
        case c0 :: cs => (assignment0 mapValues { c1 => if(cs contains c1) c0 else c1 }) ++ (indices map { (_, c0) })
      }
  }
}