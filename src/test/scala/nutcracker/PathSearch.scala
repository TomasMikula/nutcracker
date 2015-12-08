package nutcracker

import scala.language.higherKinds

import org.scalatest.FunSuite
import scalaz.Id._
import scalaz.{StreamT, MonadPlus}

class PathSearch extends FunSuite {

  type Vertex = Symbol

  val edges = 'A -> 'B ::
              'A -> 'C ::
              'B -> 'D ::
              'B -> 'E ::
              'C -> 'F ::
              'C -> 'G ::
              'D -> 'H ::
              'E -> 'A ::
              'E -> 'G ::
              'F -> 'I ::
              'G -> 'I ::
              'G -> 'K ::
              'H -> 'B ::
              'H -> 'K ::
              'I -> 'J ::
              'J -> 'F ::
              'K -> 'E ::
              Nil

  sealed trait Path
  case class Identity(v: Vertex) extends Path
  case class Cons(v: Vertex, tail: Path) extends Path

  def identity(v: Vertex): Path = Identity(v)
  def cons(v: Vertex, tail: Path): Path = Cons(v, tail)

  def path(v: Vertex, vs: Vertex*): Path =
    if(vs.isEmpty) identity(v)
    else cons(v, path(vs.head, vs.tail:_*))

  def successors(v: Vertex): List[Vertex] = edges filter { _._1 == v } map { _._2 }

  def findPath[F[_]: MonadPlus](u: Vertex, v: Vertex): F[Path] =
    findPath(u, v, Nil)

  def findPath[F[_]: MonadPlus](u: Vertex, v: Vertex, avoid: List[Vertex]): F[Path] =
    MonadPlus[F].plus[Path](
      if(u == v) MonadPlus[F].pure(identity(u)) else MonadPlus[F].empty,
      (successors(u) filter { w => w != u && !avoid.contains(w) } map { w => MonadPlus[F].map(findPath(w, v, u::avoid)){ cons(u, _) } }).foldLeft(MonadPlus[F].empty[Path])((a, b) => MonadPlus[F].plus(a, b))
    )

  test("Test path search") {

    type Stream[A] = StreamT[Id, A]

    val paths = findPath[Stream]('A, 'K).toStream.toSet

    assertResult(Set(
      path('A, 'B, 'D, 'H, 'K),
      path('A, 'B, 'E, 'G, 'K),
      path('A, 'C, 'G, 'K)
    ))(paths)
  }
}