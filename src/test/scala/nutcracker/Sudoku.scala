package nutcracker

import nutcracker.Domain._
import nutcracker.ProblemDescription._
import org.scalatest.FunSuite

class Sudoku extends FunSuite {

  type Cell = PureDomRef[Int, Set[Int]]
  type Cells = Vector[Cell]

  /** A program that sets up an empty Sudoku, that is 81 integer variables
    * and definitional constraints.
    */
  val sudoku0: ProblemDescription[Cells] = {
    for {
      // create 81 integer variables, ranging from 1 to 9
      cells <- variables[Int](81).oneOf((1 to 9).toSet)

      // numbers in each row are all different
      _ <- concat(rows(cells) map { allDifferent(_:_*) })

      // numbers in each column are all different
      _ <- concat(cols(cells) map { allDifferent(_:_*) })

      // numbers in each 3x3 square are all different
      _ <- concat(sqrs(cells) map { allDifferent(_:_*) })
    } yield cells
  }

  /** Returns function that, given Sudoku cells, returns a program
    * to enter the given number to the specified cell.
    */
  def set(i: Int, j: Int, value: Int): Cells => ProblemDescription[Unit] =
    cells => ProblemDescription.set(cells(i*9 + j), value)


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

    // ..3...612
    // .6..24..3
    // 5......8.
    // 1....9...
    // ...3.6...
    // ...7....9
    // .5......7
    // 3..84..2.
    // 846...1..

    val problem = for {
      cells <- sudoku0

      _ <- concat(Seq(
        set(0, 2, 3), set(0, 6, 6), set(0, 7, 1), set(0, 8, 2),
        set(1, 1, 6), set(1, 4, 2), set(1, 5, 4), set(1, 8, 3),
        set(2, 0, 5), set(2, 7, 8),
        set(3, 0, 1), set(3, 5, 9),
        set(4, 3, 3), set(4, 5, 6),
        set(5, 3, 7), set(5, 8, 9),
        set(6, 1, 5), set(6, 8, 7),
        set(7, 0, 3), set(7, 3, 8), set(7, 4, 4), set(7, 7, 2),
        set(8, 0, 8), set(8, 1, 4), set(8, 2, 6), set(8, 6, 1)
      ) map { _(cells) })

      solution <- fetchResults(cells)
    } yield solution

    val solutions = DFSSolver.solutions(problem).toStream.toList

    assertResult(1)(solutions.size)

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
    ))(solutions(0))
  }
}