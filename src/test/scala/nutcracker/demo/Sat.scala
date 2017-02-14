package nutcracker.demo

import nutcracker.PropagationLang._
import nutcracker._
import nutcracker.lib.bool.Bool._
import nutcracker.lib.bool._
import nutcracker.util.{FreeK, FreeKT}
import org.scalatest.FunSpec

import scalaz.Id._
import scalaz.Monad
import scalaz.std.anyVal._
import scalaz.std.vector._

class Sat extends FunSpec {
  val Prop = PropagationStore.module
  import Prop._

  val V = FinalVars[FreeK[Prop.Lang, ?], Ref]
  val B = BoolOps[FreeK[Prop.Lang, ?], Ref]
  val P = PromiseOps[FreeK[Prop.Lang, ?], Ref]

  import V._
  import B._
  import P._

  implicit val freeKMonad: Monad[FreeKT[Prop.Lang, Id, ?]] = FreeKT.freeKTMonad[Prop.Lang, Id]


  val solver = dfsSolver

  describe("A simple 3-SAT problem") {

    val problem = for {
      a <- variable[Boolean].count(4)()
      ā <- FreeK.traverse(a){ neg(_) }

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