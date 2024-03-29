package nutcracker.demo

import nutcracker.{Pattern, Propagation, Relations}
import nutcracker.Rel.Rel2
import nutcracker.data.Promise
import nutcracker.data.Promises._
import nutcracker.util.HList.{::, HNil}
import nutcracker.util.Pointers0._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import scalaz.NonEmptyList
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.syntax.monad._

class SimpleForwardInference extends AnyFunSpec {

  // Define some relations.
  // In this demo, we're going to do reasoning with symbols only, so our relations are between symbols
  object LT  extends Rel2[String, String] // a binary relation on Strings
  object LTE extends Rel2[String, String] // another binary relation on Strings

  // define some shortcuts
  type Pair = String :: String :: HNil
  type Triple = String :: String :: String :: HNil

  // a program to add some inference rules for LT and LTE
  def LtLteRules[M[_]](using relations: Relations[M]): M[Unit] = {
    import relations._

    for {
      // LT(a, b) => LTE(a, b)
      _ <- onPatternMatch(
            Pattern[Pair].build({ case (a :: b :: HNil) => NonEmptyList(LT(a, b)) }))(
            { case (a :: b :: HNil) => relate(LTE).values(a :: b :: HNil) })

      // LT(a, b), LTE(b, c) => LT(a, c)
      _ <- onPatternMatch(
            Pattern[Triple].build({ case (a :: b :: c :: HNil) => NonEmptyList(LT(a, b), LTE(b, c)) }))(
            { case (a :: b :: c :: HNil) => relate(LT).values(a :: c :: HNil) })

      // LTE(a, b), LT(b, c) => LT(a, c)
      _ <- onPatternMatch(
            Pattern[Triple].build({ case (a :: b :: c :: HNil) => NonEmptyList(LTE(a, b), LT(b, c)) }))(
            { case (a :: b :: c :: HNil) => relate(LT).values(a :: c :: HNil) })

      // LTE(a, b), LTE(b, c) => LTE(a, c)
      _ <- onPatternMatch(
            Pattern[Triple].build({ case (a :: b :: c :: HNil) => NonEmptyList(LTE(a, b), LTE(b, c)) }))(
            { case (a :: b :: c :: HNil) => relate(LTE).values(a :: c :: HNil) })

    } yield ()
  }


  describe("From a ≤ b ≤ c < d ≤ e") {

    def problem[M[_]](implicit P: Propagation[M], R: Relations[M]): M[P.Out[Promise[Unit]]] = {
      import R.{M, onPatternMatch, relate}
      import P.out

      // set up the initial relations
      relate(LTE).values("a" :: "b" :: HNil) >>
      relate(LTE).values("b" :: "c" :: HNil) >>
      relate(LT ).values("c" :: "d" :: HNil) >>
      relate(LTE).values("d" :: "e" :: HNil) >>
      // add inference rules to the mix
      LtLteRules[M] >>
      // observe when a < e is inferred
      (for {
        pr <- promise[Unit]()
        _ <- onPatternMatch(
          Pattern[Pair]
            .where({ case (x :: y :: HNil) => (x -> "a") :: (y -> "e") :: HNil })
            .build({ case (x :: y :: HNil) => NonEmptyList(LT(x, y)) }))
            .apply({ _ => pr.complete(()) })
      } yield out(pr))
    }

    it("should follow that a < e") {
      val res = nutcracker.toolkit.PropRelToolkit.run([m[_]] => (P: Propagation[m], R: Relations[m]) => problem[m](P, R))
      res should be (Promise.Completed(()))
    }
  }
}
