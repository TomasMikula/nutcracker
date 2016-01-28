package nutcracker.demo

import nutcracker.BFSSolver
import nutcracker.BranchLang._
import nutcracker.CostLang._
import nutcracker.PromiseLang._
import nutcracker.algebraic.NonDecreasingMonoid
import nutcracker.util.free.{FreeK, InjectK}
import org.scalatest.FunSuite

import scala.annotation.tailrec
import scala.language.higherKinds
import scalaz.Id._
import scalaz.{Ordering, StreamT}

class PathSearch extends FunSuite {
  implicit val positiveIntMonoid: NonDecreasingMonoid[Int] = new NonDecreasingMonoid[Int] {
    def zero: Int = 0
    def append(a: Int, b: => Int): Int = a + b
    def order(x: Int, y: Int): Ordering = Ordering.fromInt(x - y)
  }

  val solver: BFSSolver[Int] = new BFSSolver[Int]

  type Lang[K[_], A] = solver.lang.Vocabulary[K, A]

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

  def findPath(u: Vertex, v: Vertex): FreeK[Lang, Promised[Path]] = for {
    pr <- promiseF[Path].inject[Lang]
    _ <- findPath(Nil, u, v, pr)
  } yield pr

  def findPath(visited: List[Vertex], u: Vertex, v: Vertex, pr: Promised[Path]): FreeK[Lang, Unit] = {
    branch(
      zeroLengthPaths(visited, u, v, pr),
      nonZeroLengthPaths(visited, u, v, pr)
    )
  }

  def zeroLengthPaths(visited: List[Vertex], u: Vertex, v: Vertex, pr: Promised[Path]): FreeK[Lang, Unit] = {
    if(u == v) completeF(pr, revPath(u::visited)).inject[Lang]
    else branch()
  }

  def nonZeroLengthPaths(visited: List[Vertex], u: Vertex, v: Vertex, pr: Promised[Path]): FreeK[Lang, Unit] = {
    implicit val inj = implicitly[InjectK[solver.lang.CostL, Lang]]
    val branches = successors(u) filter {
      case (c, w) => w != u && !visited.contains(w)
    } map {
      case (c, w) => costF(c) >>> findPath(u::visited, w, v, pr)
    }
    branch(branches:_*)
  }

  test("Test path search") {

    type Stream[A] = StreamT[Id, A]

    val paths = solver.solutions(findPath('A, 'K)).toStream.toList
//    val paths = findPath[Stream]('A, 'K).toStream.toSet

    assertResult(List(
      (path('A, 'B, 'D, 'H, 'K), 15),
      (path('A, 'B, 'E, 'G, 'K), 16),
      (path('A, 'C, 'G, 'K), 17)
    ))(paths)
  }

  private def branch(ks: FreeK[Lang, Unit]*): FreeK[Lang, Unit] = {
    implicit val inj = implicitly[InjectK[solver.lang.BranchL, Lang]]
    addBranchingF[StreamT[Id, ?], Lang](StreamT.fromIterable(ks))
  }
}