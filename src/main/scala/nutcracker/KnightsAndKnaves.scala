package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._

object KnightsAndKnaves extends App {

  val problem1 = for {
    a <- variable[Boolean]()
    b <- variable[Boolean]()
    c <- variable[Boolean]()

    // b says (a says knave(a))
    _ <- presume(b =?= (a =?= neg(a)))

    // c says knave(b)
    _ <- presume(c =?= neg(b))

    sol <- fetchResults(Vector(a, b, c))
  } yield sol

  val solutions1 = DFSSolver.solutions(problem1).toStream.toList
  solutions1 foreach { println(_) }

  println

  val problem2 = for {
    a <- variable[Boolean]()
    b <- variable[Boolean]()

    // a says (knave(a) ∨ knave(b))
    _ <- presume(a =?= (neg(a) ∨ neg(b)))

    sol <- fetchResults(Vector(a, b))
  } yield sol

  val solutions2 = DFSSolver.solutions(problem2).toStream.toList
  solutions2 foreach { println(_) }
}
