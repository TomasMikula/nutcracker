package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._
import shapeless.Nat._
import shapeless.Sized

object SatTest extends App {

  val problem = for {
    a1 <- variable[Boolean]()
    a2 <- variable[Boolean]()
    a3 <- variable[Boolean]()
    a4 <- variable[Boolean]()
    ā1 <- not(a1)
    ā2 <- not(a2)
    ā3 <- not(a3)
    ā4 <- not(a4)
    _ <- atLeastOneTrue(a1, a2, a3)
    _ <- atLeastOneTrue(ā2, a3, ā4)
    _ <- atLeastOneTrue(ā1, a3, a4)
    _ <- atLeastOneTrue(ā1, ā3, ā4)
    _ <- atLeastOneTrue(a2, a3, ā4)
    _ <- atLeastOneTrue(a1, ā2, a4)
    _ <- atLeastOneTrue(a1, a3, a4)
    _ <- atLeastOneTrue(ā1, a2, ā3)
    sol <- fetchResults(Sized[Vector](a1, a2, a3, a4))
  } yield sol

  val n = DFSSolver.solutions(problem).foldLeft(0)((i, s) => { println(s); i + 1 })
  println(s"Found $n solutions")
}