package nutcracker.rel

import nutcracker.Pattern
import nutcracker.data.Tupled2
import nutcracker.data.bool.Bool
import nutcracker.toolkit.PropRel
import nutcracker.util.SummonHList
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable
import scalaz.{ContT, NonEmptyList, Order}
import scalaz.syntax.monad._
import shapeless.{::, HNil}

class FunctionalRelationTest extends AnyFunSuite {
  import PropRel._
  import PropRel.propagationApi.{Val => _, readOnly =>_, _}
  import PropRel.relationsApi._

  type ContU[A] = ContT[Unit, Prg, A]

  test("blah") {
    type L = Val[Bool] :: Val[Bool] :: Val[(Bool, Bool)] :: HNil

    var initializedTimes = 0
    var insertedTimes = 0
    val observed: mutable.Buffer[Val[(Bool, Bool)]] = mutable.Buffer()

    val paired = Tupled2[Bool, Bool, Val]()

    val orderVal: scalaz.Order[Val[_]] =
      (x: Val[_], y: Val[_]) => valOrder.order(x, y)

    implicit val orders: SummonHList[Order[Val[_]] :: Order[Val[_]] :: Order[Val[_]] :: HNil] =
      orderVal :: orderVal :: orderVal :: SummonHList.hnilWrapper

    def init(a: Val[Bool], b: Val[Bool]) =
      establish(paired).matching2(a, b)
        .by(Tupled2.recipe[Bool, Bool, Var, Val, Prg].andThen(_ => (initializedTimes += 1).point[Prg]))

    def observe(cps: ContU[L]): Prg[Unit] =
      cps(p => (observed.append(p.tail.tail.head): Unit).point[Prg])

    val pattern = Pattern[L].build({ case a::b::ab::HNil => NonEmptyList(paired(a, b, ab)) })

    val prg = for {
      _ <- onPatternMatch(pattern)(_ => (insertedTimes += 1).point[Prg])
      a <- newCell[Bool]
      b <- newCell[Bool]
      pc = init(a, b)
      qc = init(a, b)
      _ <- observe(pc)
      _ <- observe(qc)
    } yield ()

    val (state, _) = interpret0(prg)

    assertResult(1)(initializedTimes)
    assertResult(2)(observed.size)
    assert(observed(0) == observed(1))
    assertResult(1)(insertedTimes)
  }

}
