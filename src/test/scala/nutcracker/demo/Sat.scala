package nutcracker.demo

import nutcracker._
import nutcracker.lib.bool.Bool._
import nutcracker.lib.bool._
import nutcracker.util.FreeKT
import nutcracker.util.ops.applicative._
import org.scalatest.FunSpec

import scalaz.Id._
import scalaz.Monad
import scalaz.std.anyVal._
import scalaz.std.vector._
import scalaz.syntax.traverse._

class Sat extends FunSpec {
  import PropBranch._
  import PropBranch.branchingPropagation.{propagation => _, _}

  val B = BoolOps[Prg, Ref]
  val P = PromiseOps[Prg, Ref]

  import B._
  import P._

  implicit val freeKMonad: Monad[FreeKT[Prop.Lang, Id, ?]] = FreeKT.freeKTMonad[Prop.Lang, Id]


  val solver = dfsSolver

  describe("A simple 3-SAT problem") {

    val problem = for {
      a <- newVar[Bool].replicate(4)
      ā <- a.traverse(neg(_))

      _ <- atLeastOneTrue(a(0), a(1), a(2))
      _ <- atLeastOneTrue(ā(1), a(2), ā(3))
      _ <- atLeastOneTrue(ā(0), a(2), a(3))
      _ <- atLeastOneTrue(ā(0), ā(2), ā(3))
      _ <- atLeastOneTrue(a(1), a(2), ā(3))
      _ <- atLeastOneTrue(a(0), ā(1), a(3))
      _ <- atLeastOneTrue(a(0), a(2), a(3))
      _ <- atLeastOneTrue(ā(0), a(1), ā(2))

      r <- promiseResults(a)
    } yield r

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