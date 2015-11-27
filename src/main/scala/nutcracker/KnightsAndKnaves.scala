package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._

// From https://en.wikipedia.org/wiki/Knights_and_Knaves:
//
// The puzzles are set on a fictional island where all inhabitants
// are either knights, who always tell the truth, or knaves, who always
// lie. The puzzles involve a visitor to the island who meets small
// groups of inhabitants. Usually the aim is for the visitor to deduce
// the inhabitants' type from their statements.

object KnightsAndKnaves extends App {

  // The visitor meets three inhabitants referred to as A, B and C.
  // The visitor asks A what type he is, but does not hear A's answer.
  // B then says "A said that he is a knave" and
  // C says "Don't believe B; he is lying!"
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


  // The visitor meets inhabitants A and B and asks
  // "Are there any knaves among you?"
  // A replies "Yes."
  val problem2 = for {
    a <- variable[Boolean]()
    b <- variable[Boolean]()

    // a says (knave(a) ∨ knave(b))
    _ <- presume(a =?= (neg(a) ∨ neg(b)))

    sol <- fetchResults(Vector(a, b))
  } yield sol

  val solutions2 = DFSSolver.solutions(problem2).toStream.toList
  solutions2 foreach { println(_) }
  println


  // The visitor meets inhabitants A and B.
  // A says "We are both knaves."
  val problem3 = for {
    a <- variable[Boolean]()
    b <- variable[Boolean]()

    // a says (knave(a) ∧ knave(b))
    _ <- presume(a =?= (neg(a) ∧ neg(b)))

    sol <- fetchResults(Vector(a, b))
  } yield sol

  val solutions3 = DFSSolver.solutions(problem3).toStream.toList
  solutions3 foreach { println(_) }
  println


  // The visitor meets inhabitants A and B.
  // A says "We are the same kind."
  // B says "We are of different kinds."
  val problem4 = for {
    a <- variable[Boolean]()
    b <- variable[Boolean]()

    // a says (kind(a) = kind(b))
    _ <- presume(a =?= (a =?= b))

    // b says (kind(a) ≠ kind(b)
    _ <- presume(b =?= neg(a =?= b))

    sol <- fetchResults(Vector(a, b))
  } yield sol

  val solutions4 = DFSSolver.solutions(problem4).toStream.toList
  solutions4 foreach { println(_) }
  println
}
