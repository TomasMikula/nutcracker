package nutcracker.demo

import nutcracker.DecSet
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker._
import nutcracker.util.{FreeK, FreeKT}
import nutcracker.util.FreeK._
import org.scalatest.FunSuite

import scalaz.Id._
import scalaz.Monad
import scalaz.std.anyVal._

class Sudoku extends FunSuite {
  val Prop = PropagationStore.module
  import Prop._

  val P1 = Propagation[FreeK[Prop.Lang, ?], Ref]
  val P2 = PromiseOps[FreeK[Prop.Lang, ?], Ref]
  val V = FinalVars[FreeK[Prop.Lang, ?], Ref]

  import P1._
  import P2._
  import V._

  implicit val freeKMonad: Monad[FreeKT[Prop.Lang, Id, ?]] = FreeKT.freeKTMonad[Prop.Lang, Id]


  val solver = dfsSolver

  type Cell = Ref[DecSet[Int]]
  type Cells = Vector[Cell]

  /** A program that sets up an empty Sudoku, that is 81 integer variables
    * and definitional constraints.
    */
  val sudoku0: FreeK[Prop.Lang, Cells] = {
    for {
      // create 81 integer variables, ranging from 1 to 9
      cells <- variable[Int].count(81).oneOf((1 to 9).toSet)

      // numbers in each row are all different
      _ <- sequence_(rows(cells) map { allDifferent(_:_*) })

      // numbers in each column are all different
      _ <- sequence_(cols(cells) map { allDifferent(_:_*) })

      // numbers in each 3x3 square are all different
      _ <- sequence_(sqrs(cells) map { allDifferent(_:_*) })
    } yield cells
  }

  /** A more sophisticated Sudoku program that includes additional constraints
    * and thus reduces the amount of guessing and backtracking.
    */
  val sudoku1: FreeK[Prop.Lang, Cells] = {

    // For the given segment (row/column/3x3 square) and number,
    // keep a set of possible positions of that number in that segment.
    // When only one possible position remains, enter the number to that cell.
    def segNumConstraint(seg: Seq[Cell], x: Int): FreeK[Prop.Lang, Unit] = {
      for {
        xPos <- variable[Cell].oneOf(seg.toSet)
        _ <- sequence_(seg map { cell => valTrigger(cell) { ys =>
          if(!ys.contains(x)) fire(exclude(xPos, cell))
          else if(ys.size == 1) fire(V.set(xPos, cell))
          else sleep
        } })
        _ <- whenFinal(xPos).exec(cell => V.set(cell, x))
      } yield ()
    }

    def segConstraints(seg: Seq[Cell]): FreeK[Prop.Lang, Unit] = {
      sequence_((1 to 9) map { segNumConstraint(seg, _) })
    }

    for {
      cells <- sudoku0
      _ <- sequence_(rows(cells) map { segConstraints(_) })
      _ <- sequence_(cols(cells) map { segConstraints(_) })
      _ <- sequence_(sqrs(cells) map { segConstraints(_) })
    } yield cells
  }

  /** Returns function that, given Sudoku cells, returns a program
    * to enter the given number to the specified cell.
    */
  def set(i: Int, j: Int, value: Int): Cells => FreeK[Prop.Lang, Unit] =
    cells => V.set(cells(i*9 + j), value)


  // technicalities

  private lazy val _rows: Seq[Seq[Int]] = (0 until 9) map { i => (0 until 9) map { j => i*9 + j } }
  private lazy val _cols: Seq[Seq[Int]] = (0 until 9) map { j => (0 until 9) map { i => i*9 + j } }
  private lazy val _sqrs: Seq[Seq[Int]] =
    for {
      i <- 0 until 3
      j <- 0 until 3
    } yield for {
      k <- 0 until 3
      l <- 0 until 3
    } yield (i*3 + k)*9 + j*3 + l

  private def rows(cells: Cells): Seq[Seq[Cell]] = {
    _rows map { _ map { cells(_) } }
  }

  private def cols(cells: Cells): Seq[Seq[Cell]] = {
    _cols map { _ map { cells(_) } }
  }

  private def sqrs(cells: Cells): Seq[Seq[Cell]] = {
    _sqrs map { _ map { cells(_) } }
  }


  // test cases

  test("Sample Sudoku") {

    // ..3...61.
    // .6..24..3
    // 5......8.
    // 1....9...
    // ...3.6...
    // ...7....9
    // .5......7
    // 3..84....
    // 846...1..

    // we test both the simple and the advanced sudoku program
    val problems = Seq(sudoku0, sudoku1) map { sudoku =>
      for {
        cells <- sudoku

        _ <- sequence_(Seq(
          set(0, 2, 3), set(0, 6, 6), set(0, 7, 1),
          set(1, 1, 6), set(1, 4, 2), set(1, 5, 4), set(1, 8, 3),
          set(2, 0, 5), set(2, 7, 8),
          set(3, 0, 1), set(3, 5, 9),
          set(4, 3, 3), set(4, 5, 6),
          set(5, 3, 7), set(5, 8, 9),
          set(6, 1, 5), set(6, 8, 7),
          set(7, 0, 3), set(7, 3, 8), set(7, 4, 4),
          set(8, 0, 8), set(8, 1, 4), set(8, 2, 6), set(8, 6, 1)
        ) map {
          _ (cells)
        })

        solution <- promiseResults(cells)
      } yield solution
    }

    val solutions = problems map { solver.allSolutions1(_) }

    // both programs should produce a unique and correct solution
    solutions foreach { case (sols, failureCount) =>
      assertResult(1)(sols.size)
      assertResult(Vector(
        4,9,3,5,7,8,6,1,2,
        7,6,8,1,2,4,5,9,3,
        5,1,2,9,6,3,7,8,4,
        1,3,7,4,5,9,2,6,8,
        9,2,5,3,8,6,4,7,1,
        6,8,4,7,1,2,3,5,9,
        2,5,9,6,3,1,8,4,7,
        3,7,1,8,4,5,9,2,6,
        8,4,6,2,9,7,1,3,5
      ))(sols.head)
    }

    assert(solutions(1)._2 <= solutions(0)._2, "advanced sudoku program should not increase the number of backtrackings")

    // actually, on this sample puzzle the advanced
    // program eliminates backtracking altogether
    assertResult(0)(solutions(1)._2)
  }
}
