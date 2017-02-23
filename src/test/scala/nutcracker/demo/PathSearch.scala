package nutcracker.demo

import nutcracker._
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.{FreeKT, InjectK}
import org.scalatest.FunSuite
import scala.annotation.tailrec
import scalaz.Id._
import scalaz.{Equal, Monad, Ordering, StreamT}

class PathSearch extends FunSuite {

  implicit val positiveIntMonoid: NonDecreasingMonoid[Int] = new NonDecreasingMonoid[Int] {
    def zero: Int = 0
    def append(a: Int, b: => Int): Int = a + b
    def order(x: Int, y: Int): Ordering = Ordering.fromInt(x - y)
  }

  val propBranchCost = new PropBranchCost[Int]
  import propBranchCost._

  val solver = bfsSolver

  // not sure why scalac is not able to find this itself
  implicit val injC = implicitly[InjectK[solver.lang.CostL, Vocabulary]]

  val P = PromiseOps[Prg, Ref]
  val B = Branching[Prg, Ref]
  val C = CostOps[Prg]

  implicit val freeKMonad: Monad[Prg] = FreeKT.freeKTMonad[Vocabulary, Id]

  import P._
  import B._
  import C._


  type Vertex = Symbol

  val edges = List(
    'A -> ((1, 'B)),
    'A -> ((9, 'C)),
    'B -> ((3, 'D)),
    'B -> ((4, 'E)),
    'C -> ((5, 'F)),
    'C -> ((6, 'G)),
    'D -> ((7, 'H)),
    'E -> ((8, 'A)),
    'E -> ((9, 'G)),
    'F -> ((0, 'I)),
    'G -> ((1, 'I)),
    'G -> ((2, 'K)),
    'H -> ((3, 'B)),
    'H -> ((4, 'K)),
    'I -> ((5, 'J)),
    'J -> ((6, 'F)),
    'K -> ((7, 'E))
  )


  sealed trait Path
  case class Identity(v: Vertex) extends Path {
    override def toString = s"$v"
  }
  case class Cons(v: Vertex, tail: Path) extends Path {
    override def toString = s"$v->$tail"
  }
  object Path {
    implicit val equalInstance: Equal[Path] = new Equal[Path] {
      def equal(p: Path, q: Path): Boolean = p == q
    }
  }

  def identity(v: Vertex): Path = Identity(v)
  def cons(v: Vertex, tail: Path): Path = Cons(v, tail)

  def path(v: Vertex, vs: Vertex*): Path =
    if(vs.isEmpty) identity(v)
    else cons(v, path(vs.head, vs.tail:_*))

  def revPath(vs: List[Vertex]): Path = {
    @tailrec def go(vs: List[Vertex], tail: Path): Path = vs match {
      case Nil => tail
      case v::more => go(more, cons(v, tail))
    }
    go(vs.tail, identity(vs.head))
  }

  def successors(v: Vertex): List[(Int, Vertex)] = edges filter { _._1 == v } map { _._2 }

  def findPath(u: Vertex, v: Vertex): Prg[Ref[Promise[Path]]] = for {
    pr <- promise[Path]
    _ <- findPath(Nil, u, v, pr)
  } yield pr

  def findPath(visited: List[Vertex], u: Vertex, v: Vertex, pr: Ref[Promise[Path]]): Prg[Unit] = {
    branchAndExec(
      zeroLengthPaths(visited, u, v, pr),
      nonZeroLengthPaths(visited, u, v, pr)
    )
  }

  def zeroLengthPaths(visited: List[Vertex], u: Vertex, v: Vertex, pr: Ref[Promise[Path]]): Prg[Unit] = {
    if(u == v) complete(pr, revPath(u::visited))
    else branchAndExec()
  }

  def nonZeroLengthPaths(visited: List[Vertex], u: Vertex, v: Vertex, pr: Ref[Promise[Path]]): Prg[Unit] = {
    val branches = successors(u) filter {
      case (c, w) => w != u && !visited.contains(w)
    } map {
      case (c, w) => cost(c) >> findPath(u::visited, w, v, pr)
    }
    branchAndExec(branches:_*)
  }

  test("Test path search") {

    type Stream[A] = StreamT[Id, A]

    val paths = solver.solutions(findPath('A, 'K)).toStream.toList

    assertResult(List(
      (path('A, 'B, 'D, 'H, 'K), 15),
      (path('A, 'B, 'E, 'G, 'K), 16),
      (path('A, 'C, 'G, 'K), 17)
    ))(paths)
  }
}