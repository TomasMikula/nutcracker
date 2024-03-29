package nutcracker.demo

import nutcracker.data.bool.Bool._
import nutcracker.data.bool._
import nutcracker.ops.Ops._
import nutcracker.util.ops.applicative._
import org.scalatest.funspec.AnyFunSpec
import scalaz.Traverse
import scalaz.std.anyVal._
import scalaz.std.vector._
import scalaz.syntax.monad._

class Sat extends AnyFunSpec {
  import nutcracker.toolkit.PropBranchToolkit.instance._
  import nutcracker.toolkit.PropBranchToolkit.instance.branchingApi.{M => _, propagation => _, _}
  import nutcracker.data.Promises._

  val B = BoolOps[Prg]
  import B._


  describe("A simple 3-SAT problem") {

    val problem = for {
      a <- newVar[Bool].replicate(4)
      ā <- Traverse[Vector].traverse(a)(neg(_))

      _ <- atLeastOneTrue(a(0), a(1), a(2))
      _ <- atLeastOneTrue(ā(1), a(2), ā(3))
      _ <- atLeastOneTrue(ā(0), a(2), a(3))
      _ <- atLeastOneTrue(ā(0), ā(2), ā(3))
      _ <- atLeastOneTrue(a(1), a(2), ā(3))
      _ <- atLeastOneTrue(a(0), ā(1), a(3))
      _ <- atLeastOneTrue(a(0), a(2), a(3))
      _ <- atLeastOneTrue(ā(0), a(1), ā(2))

      r <- promiseResults(a)
    } yield r.asVal

    val solutions = solveDfs(problem).asLazyList.toList

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