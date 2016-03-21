package nutcracker.demo

import nutcracker.PropagationLang._
import nutcracker._
import nutcracker.lib.bool.BoolDomain._
import nutcracker.lib.bool._
import org.scalatest.FunSpec

import scala.language.higherKinds
import scalaz.std.vector._

class Sat extends FunSpec {
  val solver = PropagationStore.dfsSolver

  describe("A simple 3-SAT problem") {

    val problem = (for {
      a <- variable[Boolean].count(4)()
      ā <- traverse(a){ neg(_) }

      _ <- atLeastOneTrue(a(0), a(1), a(2))
      _ <- atLeastOneTrue(ā(1), a(2), ā(3))
      _ <- atLeastOneTrue(ā(0), a(2), a(3))
      _ <- atLeastOneTrue(ā(0), ā(2), ā(3))
      _ <- atLeastOneTrue(a(1), a(2), ā(3))
      _ <- atLeastOneTrue(a(0), ā(1), a(3))
      _ <- atLeastOneTrue(a(0), a(2), a(3))
      _ <- atLeastOneTrue(ā(0), a(1), ā(2))

    } yield a) >>>= { promiseResults(_) }

    val solutions = solver.solutions(problem).toStream.toList

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