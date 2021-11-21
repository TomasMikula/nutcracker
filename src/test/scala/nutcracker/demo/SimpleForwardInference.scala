package nutcracker.demo

import nutcracker.Pattern
import nutcracker.Rel.Rel2
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._
import scalaz.{NonEmptyList, Order}
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.syntax.monad._
import shapeless.{::, HNil}

class SimpleForwardInference extends AnyFunSpec {
  import nutcracker.toolkit.PropRelToolkit.{instance => tk}
  import tk._
  import relationsApi._
  import nutcracker.data.Promises._


  // Define some relations.
  // In this demo, we're going to do reasoning with symbols only, so our relations are between symbols
  object LT  extends Rel2[Symbol, Symbol] // a binary relation on Symbols
  object LTE extends Rel2[Symbol, Symbol] // another binary relation on Symbols

  // define some shortcuts
  type Pair = Symbol :: Symbol :: HNil
  type Triple = Symbol :: Symbol :: Symbol :: HNil

  // to be able to index Symbol values in a relational database, we need to define an ordering on Symbols
  implicit val symbolOrdering: Order[Symbol] = Order.orderBy(_.name)

  // a program to add some inference rules for LT and LTE
  val LtLteRules: Prg[Unit] = for {
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


  describe("From a ≤ b ≤ c < d ≤ e") {

    val problem =
      // set up the initial relations
      relate(LTE).values('a :: 'b :: HNil) >>
      relate(LTE).values('b :: 'c :: HNil) >>
      relate(LT ).values('c :: 'd :: HNil) >>
      relate(LTE).values('d :: 'e :: HNil) >>
      // add inference rules to the mix
      LtLteRules >>
      // observe when a < e is inferred
      (for {
        pr <- promise[Unit]()
        _ <- onPatternMatch(
          Pattern[Pair].where({ case (x :: y :: HNil) => (x -> 'a) :: (y -> 'e) :: HNil }).build({ case (x :: y :: HNil) => NonEmptyList(LT(x, y)) }))(
          { _ => complete(pr, ()) })
      } yield pr)

    it("should follow that a < e") {
      val (s, promise) = interpret(problem, tk.empty)
      fetchResult(promise, s) should be (Some(()))
    }
  }
}
