package nutcracker.demo

import algebra.Order
import algebra.std.string._
import monocle._
import nutcracker.rel.Rel.Rel2
import nutcracker.util.free.Interpreter._
import nutcracker.{PropagationLang, PropagationStore}
import nutcracker.PropagationLang._
import nutcracker.rel.{Pattern, RelDB, RelLang}
import nutcracker.rel.RelLang._
import nutcracker.util.free._
import org.scalatest.{Matchers, FunSpec}
import scalaz.NonEmptyList
import shapeless.{::, HNil}

import scala.language.higherKinds

class SimpleForwardInference extends FunSpec with Matchers {

  // à la carte composition of the desired instruction set and the state it operates on
  type Lang[K[_], A] = CoproductK[RelLang, PropagationLang, K, A]
  type State[K[_]] = ProductK[RelDB, PropagationStore, K]
  type Dirty[K[_]] = ProductK[AlwaysClean, PropagationStore.DirtyThings, K]

  // summon an interpreter for the above language
  val interpreter: Interpreter.Aux[Lang, State, Dirty] = implicitly[Interpreter.Aux[Lang, State, Dirty]]

  // definition of the composed empty state
  def emptyState[K[_]]: State[K] = {
    import ProductK._
    RelDB.empty[K] :*: PropagationStore.empty[K]
  }

  // lens into state to pull out the PromiseStore
  def propStore0[K[_]]: Lens[State[K], PropagationStore[K]] = implicitly[Lens[State[K], PropagationStore[K]]]
  val propStore = propStore0[FreeK[Lang, ?]]


  // Define some relations.
  // In this demo, we're going to do reasoning with symbols only, so our relations are between symbols
  object LT  extends Rel2[Symbol, Symbol] // a binary relation on Symbols
  object LTE extends Rel2[Symbol, Symbol] // another binary relation on Symbols

  // define some shortcuts
  type Pair = Symbol :: Symbol :: HNil
  type Triple = Symbol :: Symbol :: Symbol :: HNil

  // to be able to index Symbol values in a relational database, we need to define an ordering on Symbols
  implicit val symbolOrdering: Order[Symbol] = Order[String].on[Symbol](_.name)

  // a program to add some inference rules for LT and LTE
  val LtLteRules: FreeK[Lang, Unit] = (for {
    // LT(a, b) => LTE(a, b)
    _ <- onPatternMatchF(
           Pattern[Pair].build({ case (a :: b :: HNil) => NonEmptyList(LT(a, b)) }))(
           { case (a :: b :: HNil) => relateF(LTE).values(a :: b :: HNil) })

    // LT(a, b), LTE(b, c) => LT(a, c)
    _ <- onPatternMatchF(
           Pattern[Triple].build({ case (a :: b :: c :: HNil) => NonEmptyList(LT(a, b), LTE(b, c)) }))(
           { case (a :: b :: c :: HNil) => relateF(LT).values(a :: c :: HNil) })

    // LTE(a, b), LT(b, c) => LT(a, c)
    _ <- onPatternMatchF(
           Pattern[Triple].build({ case (a :: b :: c :: HNil) => NonEmptyList(LTE(a, b), LT(b, c)) }))(
           { case (a :: b :: c :: HNil) => relateF(LT).values(a :: c :: HNil) })

    // LTE(a, b), LTE(b, c) => LTE(a, c)
    _ <- onPatternMatchF(
           Pattern[Triple].build({ case (a :: b :: c :: HNil) => NonEmptyList(LTE(a, b), LTE(b, c)) }))(
           { case (a :: b :: c :: HNil) => relateF(LTE).values(a :: c :: HNil) })

  } yield ()).inject[Lang]


  describe("From a ≤ b ≤ c < d ≤ e") {

    val problem =
      // set up the initial relations
      relateF(LTE).values('a :: 'b :: HNil) >>
      relateF(LTE).values('b :: 'c :: HNil) >>
      relateF(LT ).values('c :: 'd :: HNil) >>
      relateF(LTE).values('d :: 'e :: HNil) >>>
      // add inference rules to the mix
      LtLteRules >>
      // observe when a < e is inferred
      (for {
        pr <- promiseF[Unit].inject[Lang]
        _ <- onPatternMatchF(
          Pattern[Pair].where({ case (x :: y :: HNil) => (x -> 'a) :: (y -> 'e) :: HNil }).build({ case (x :: y :: HNil) => NonEmptyList(LT(x, y)) }))(
          { _ => completeF(pr, ()).inject[Lang] })
      } yield pr)

    it("should follow that a < e") {
      val (s, promise) = interpreter.runFree(emptyState[FreeK[Lang, ?]], problem)
      propStore.get(s).fetchResult(promise) should be (Some(()))
    }
  }
}
