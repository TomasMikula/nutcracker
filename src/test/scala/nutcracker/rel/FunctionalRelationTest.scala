package nutcracker.rel

import nutcracker.PropRel
import nutcracker.lib.Tupled2
import nutcracker.lib.bool.Bool
import org.scalatest.FunSuite
import scala.collection.mutable
import scalaz.{ContT, NonEmptyList}
import scalaz.syntax.monad._
import shapeless.{::, HNil}

class FunctionalRelationTest extends FunSuite {
  import PropRel._
  import PropRel.propagationApi._
  import PropRel.relationsApi._

  type ContU[A] = ContT[Prg, Unit, A]

  test("blah") {
    type L = Ref[Bool] :: Ref[Bool] :: Ref[(Bool, Bool)] :: HNil

    var initializedTimes = 0
    var insertedTimes = 0
    val observed: mutable.Buffer[Ref[(Bool, Bool)]] = mutable.Buffer()

    val paired = Tupled2[Bool, Bool, Ref]

    def init(a: Ref[Bool], b: Ref[Bool]) =
      establish(paired).matching2(a, b)
        .by(Tupled2.recipe[Bool, Bool, Ref, Prg].andThen(_ => (initializedTimes += 1).point[Prg]))

    def observe(cps: ContU[L]): Prg[Unit] =
      cps(p => observed.append(p.tail.tail.head).point[Prg])

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
