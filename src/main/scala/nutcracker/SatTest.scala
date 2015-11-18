package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool.Disjunction
import shapeless.Nat._
import shapeless.Sized

object SatTest extends App {

  val problem = for {
    x1 <- variable[Boolean]()
    x2 <- variable[Boolean]()
    x3 <- variable[Boolean]()
    x4 <- variable[Boolean]()
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](true,  true,  true )), Sized[Vector](x1, x2, x3)) //  x1 ∨  x2 ∨  x3
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](false, true,  false)), Sized[Vector](x2, x3, x4)) // ¬x2 ∨  x3 ∨ ¬x4
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](false, true,  true )), Sized[Vector](x1, x3, x4)) // ¬x1 ∨  x3 ∨  x4
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](false, false, false)), Sized[Vector](x1, x3, x4)) // ¬x1 ∨ ¬x3 ∨ ¬x4
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](true,  true,  false)), Sized[Vector](x2, x3, x4)) //  x2 ∨  x3 ∨ ¬x4
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](true,  false, true )), Sized[Vector](x1, x2, x4)) //  x1 ∨ ¬x2 ∨  x4
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](true,  true,  true )), Sized[Vector](x1, x3, x4)) //  x1 ∨  x3 ∨  x4
    _ <- vectorConstraint(Disjunction[_3](Sized[Vector](false, true,  false)), Sized[Vector](x1, x2, x3)) // ¬x1 ∨  x2 ∨ ¬x3
    sol <- fetchResults(Sized[Vector](x1, x2, x3, x4))
  } yield sol

  val n = DFSSolver.solutions(problem).foldLeft(0)((i, s) => { println(s); i + 1 })
  println(s"Found $n solutions")
}