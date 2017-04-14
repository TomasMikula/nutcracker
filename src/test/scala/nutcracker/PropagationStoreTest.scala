package nutcracker

import nutcracker.DecSet._
import nutcracker.log._
import nutcracker.ops._
import org.scalatest.FunSuite
import scalaz.syntax.monad._

class PropagationStoreTest extends FunSuite {
  import PropBranchToolkit.instance._

  val P = propagationApi

  test("diff accumulation") {
    val prg = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      log <- newLog[Removed[Int]]()
      _   <- P.observe(ref).by(_ => P.sleep(P.continually((s: DecSet[Int], d: Removed[Int]) => log.write(d))))
      _   <- ref.exclude(DecSet(1, 2))
      _   <- ref.exclude(DecSet(4, 5))
    } yield log

    val (store, logRef) = interpret(prg, empty[Prg])
    val log = fetch(logRef, store)
    assertResult(List(Removed(Set(1, 2, 4, 5))))(log)
  }

  test("delta is cleared after being handled") {
    val prg1 = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      log <- newLog[Removed[Int]]()
      _   <- P.observe(ref).by(_ => P.sleep(P.continually((s: DecSet[Int], d: Removed[Int]) => log.write(d))))
      _   <- ref.exclude(DecSet(1, 2))
    } yield (ref, log)

    val (store1, (ref, logRef)) = interpret(prg1, empty[Prg])
    val log1 = fetch(logRef, store1)
    assertResult(List(Removed(Set(1, 2))))(log1)

    val prg2 = ref.exclude(DecSet(4, 5))
    val store2 = interpret(prg2, store1)._1
    val log2 = fetch(logRef, store2)
    assertResult(List(Removed(Set(4, 5)), Removed(Set(1, 2))))(log2)
  }

  test("delta is published when registering new trigger") {
    val prg = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      _   <- ref.exclude(DecSet(4, 5, 6, 7)) // should not be logged
      log <- newLog[Removed[Int]]()
      _   <- P.observe(ref).by(_ => P.sleep(P.continually((s: DecSet[Int], d: Removed[Int]) => log.write(d))))
      _   <- ref.exclude(DecSet(2, 3, 4, 5)) // only removal of {2, 3} should be logged
    } yield log

    val (store, logRef) = interpret(prg, empty[Prg])
    val log = fetch(logRef, store)
    assertResult(List(Removed(Set(2, 3))))(log)
  }

}
