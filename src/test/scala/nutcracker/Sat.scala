package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._
import org.scalatest.FunSpec

import scalaz.std.vector._
import scalaz.syntax.traverse._

class Sat extends FunSpec {

  describe("A simple 3-SAT problem") {

    val problem = for {
      a <- variables[Boolean](4)
      ā <- a traverseU { neg(_) }

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

    it("should have 4 solutions") {
      assertResult(4)(solutions.size)
    }

    they("should be these ones") {
      this.assertResult(Set(
        Vector(true, true, true, false),
        Vector(false, true, true, true),
        Vector(false, false, true, true),
        Vector(false, false, true, false)
      ))(solutions.toSet)
    }

  }
}