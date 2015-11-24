package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._
import scalaz.syntax.traverse._
import scalaz.std.vector._

object SatTest extends App {

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

  val n = DFSSolver.solutions(problem).foldLeft(0)((i, s) => { println(s); i + 1 })
  println(s"Found $n solutions")
}