package nutcracker

import nutcracker.data.{CellSet, Promise, Promises}
import nutcracker.toolkit.PropagationToolkit
import org.scalatest.FunSuite
import org.scalatest.Matchers._
import scala.collection.mutable
import scalaz.std.anyVal._
import scalaz.syntax.monad._

class CellSetTest extends FunSuite {
  val Prop = PropagationToolkit.instance
  import Prop._

  val P = Prop.propagationApi

  test("autoclean") {
    val observed = mutable.Buffer[Var[Promise[Int]]]()

    val prg = for {
      res <- CellSet.init[Promise[Int]]()
      _ <- P.observe(res).by(_ => P.sleep(P.continually((d, δ) => δ.value.foreach(r => observed += r).point[Prg])))
      p1 <- P.newCell[Promise[Int]](Promise.Conflict)
      p2 <- Promises.promise[Int]()
      _ <- Promises.complete(p2, 42)
      _ <- CellSet.insert(p1, res)
      _ <- CellSet.insert(p2, res)
    } yield (res, p2)

    val (s, (res, p2)) = interpret0(prg)

    // only p2 should be is observed, since p1 was failed on insertion
    observed.size should be (1)
    observed(0) should be (p2)

    // fail p1
    val prg2 = Promises.complete(p2, 7)
    val s2 = interpret(prg2, s)._1

    // check that p1 was auto-cleaned
    val resSet = fetch(res, s2)
    resSet.size should be (0)
  }

}