package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._
import scalaz.syntax.traverse._
import scalaz.std.vector._

object SatTest extends App {

  // description of a simple SAT problem
  // (doesn't have to be 3-SAT)
  val problem = for {
    a <- variables[Boolean](4)
    ā <- a traverseU { not(_) }
    _ <- atLeastOneTrue(a(0), a(1), a(2))
    _ <- atLeastOneTrue(ā(1), a(2), ā(3))
    _ <- atLeastOneTrue(ā(0), a(2), a(3))
    _ <- atLeastOneTrue(ā(0), ā(2), ā(3))
    _ <- atLeastOneTrue(a(1), a(2), ā(3))
    _ <- atLeastOneTrue(a(0), ā(1), a(3))
    _ <- atLeastOneTrue(a(0), a(2), a(3))
    _ <- atLeastOneTrue(ā(0), a(1), ā(2))
    sol <- fetchResults(a)
  } yield sol

  val solutions = DFSSolver.solutions(problem).toStream.toList


  val expected = Set(
    Vector(true, true, true, false),
    Vector(false, true, true, true),
    Vector(false, false, true, true),
    Vector(false, false, true, false)
  )

  assert(solutions.size == 4)
  assert(solutions.toSet == expected)

  solutions foreach { println(_) }
  println(s"Found ${solutions.size} solutions")
}