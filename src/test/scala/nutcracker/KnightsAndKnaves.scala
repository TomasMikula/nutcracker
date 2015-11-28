package nutcracker

import nutcracker.ProblemDescription._
import nutcracker.theories.bool.BoolDomain._
import nutcracker.theories.bool._
import org.scalatest.FreeSpec

// From https://en.wikipedia.org/wiki/Knights_and_Knaves:
//
// The puzzles are set on a fictional island where all inhabitants
// are either knights, who always tell the truth, or knaves, who always
// lie. The puzzles involve a visitor to the island who meets small
// groups of inhabitants. Usually the aim is for the visitor to deduce
// the inhabitants' type from their statements.

class KnightsAndKnaves extends FreeSpec {

  "Problem 1" - {
    // The visitor meets three inhabitants referred to as A, B and C.
    // The visitor asks A what type he is, but does not hear A's answer.
    // B then says "A said that he is a knave" and
    // C says "Don't believe B; he is lying!"
    val problem = for {
      a <- variable[Boolean]()
      b <- variable[Boolean]()
      c <- variable[Boolean]()

      // b says (a says knave(a))
      _ <- presume(b =?= (a =?= neg(a)))

      // c says knave(b)
      _ <- presume(c =?= neg(b))

      sol <- fetchResults(a, b, c)
    } yield sol

    val solutions = DFSSolver.solutions(problem).toStream.toList

    "should have 2 solutions" in {
      assertResult(2)(solutions.size)
    }

    solutions foreach { sol =>
      ("Solution: " + sol) - {
        "B is a knave" in { assertResult(false)(sol(1)) }
        "C is a knight" in { assertResult(true)(sol(2)) }
      }
    }
  }

  "Are there any knaves?" - {
    // The visitor meets inhabitants A and B and asks
    // "Are there any knaves among you?"
    // A replies "Yes."
    val problem = for {
      a <- variable[Boolean]()
      b <- variable[Boolean]()

      // a says (knave(a) ∨ knave(b))
      _ <- presume(a =?= (neg(a) ∨ neg(b)))

      sol <- fetchResults(a, b)
    } yield sol

    val solutions = DFSSolver.solutions(problem).toStream.toList

    "should have a unique solution" - {
      "check" in { assertResult(1)(solutions.size) }

      val sol = solutions(0)
      ("Solution: " + sol) - {
        "A is a knight" in { assertResult(true)(sol(0)) }
        "B is a knave" in { assertResult(false)(sol(1)) }
      }
    }
  }

  "Both knaves" - {
    // The visitor meets inhabitants A and B.
    // A says "We are both knaves."
    val problem = for {
      a <- variable[Boolean]()
      b <- variable[Boolean]()

      // a says (knave(a) ∧ knave(b))
      _ <- presume(a =?= (neg(a) ∧ neg(b)))

      sol <- fetchResults(a, b)
    } yield sol

    val solutions = DFSSolver.solutions(problem).toStream.toList

    "should have a unique solution" - {
      "check" in { assertResult(1)(solutions.size) }

      val sol = solutions(0)
      ("Solution: " + sol) - {
        "A is a knave" in { assertResult(false)(sol(0)) }
        "B is a knight" in { assertResult(true)(sol(1)) }
      }
    }
  }


  "Same or Different" - {
    // The visitor meets inhabitants A and B.
    // A says "We are the same kind."
    // B says "We are of different kinds."
    val problem = for {
      a <- variable[Boolean]()
      b <- variable[Boolean]()

      // a says (kind(a) = kind(b))
      _ <- presume(a =?= (a =?= b))

      // b says (kind(a) ≠ kind(b)
      _ <- presume(b =?= neg(a =?= b))

      sol <- fetchResults(a, b)
    } yield sol

    val solutions = DFSSolver.solutions(problem).toStream.toList

    "should have a unique solution" - {
      "check" in { assertResult(1)(solutions.size) }

      val sol = solutions(0)
      ("Solution: " + sol) - {
        "A is a knave" in { assertResult(false)(sol(0)) }
        "B is a knight" in { assertResult(true)(sol(1)) }
      }
    }
  }
}