package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.PartialSolution._

import shapeless._

object PathSearchTest {

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

//  Paths from A to K:
//  A B D H K
//  A B E G K
//  A C G K

  type Vertex = Symbol

  sealed trait Path
  case class Identity(v: Vertex) extends Path
  case class Cons(v: Vertex, tail: Path) extends Path

  def successors(v: Vertex): List[Vertex] = edges filter { _._1 == v } map { _._2 }

  def findPath(u: Vertex, v: Vertex): ProblemDescription[Path] =
    findPath(u, v, Nil)

  def findPath(u: Vertex, v: Vertex, avoid: List[Vertex]): ProblemDescription[Path] =
    branch(
        () => if(u == v) pure(Identity(u)) else empty,
        () => branch(() => successors(u) filter { w => w != u && !avoid.contains(w) } map { w => findPath(w, v, u::avoid) map { Cons(u, _) } }))

}