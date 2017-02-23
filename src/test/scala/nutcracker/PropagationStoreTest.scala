package nutcracker

import org.scalatest.FunSuite
import nutcracker.DecSet._
import nutcracker.log._
import nutcracker.ops._
import nutcracker.Trigger._

class PropagationStoreTest extends FunSuite {
  import PropBranch._

  val P = Propagation[Prg, Ref]
  import P._

  test("diff accumulation") {
    val prg = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      log <- newLog[Prg, Ref, Diff[Set[Int]]]
      _   <- observe(ref).by(_ => sleep(continually((s: DecSet[Int], d: Diff[Set[Int]]) => log.write[Prg](d))))
      _   <- ref.exclude(DecSet(1, 2))
      _   <- ref.exclude(DecSet(4, 5))
    } yield log

    val (store, logRef) = interpreter(prg)(emptyState[Prg])
    val log = fetch(store)(logRef)
    assertResult(List(Diff(Set(1, 2, 4, 5))))(log)
  }

  test("delta is cleared after being handled") {
    val prg1 = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      log <- newLog[Prg, Ref, Diff[Set[Int]]]
      _   <- observe(ref).by(_ => sleep(continually((s: DecSet[Int], d: Diff[Set[Int]]) => log.write[Prg](d))))
      _   <- ref.exclude(DecSet(1, 2))
    } yield (ref, log)

    val (store1, (ref, logRef)) = interpreter(prg1)(emptyState[Prg])
    val log1 = fetch(store1)(logRef)
    assertResult(List(Diff(Set(1, 2))))(log1)

    val prg2 = ref.exclude(DecSet(4, 5))
    val store2 = interpreter(prg2)(store1)._1
    val log2 = fetch(store2)(logRef)
    assertResult(List(Diff(Set(4, 5)), Diff(Set(1, 2))))(log2)
  }

  test("delta is published when registering new trigger") {
    val prg = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      _   <- ref.exclude(DecSet(4, 5, 6, 7)) // should not be logged
      log <- newLog[Prg, Ref, Diff[Set[Int]]]
      _   <- observe(ref).by(_ => sleep(continually((s: DecSet[Int], d: Diff[Set[Int]]) => log.write[Prg](d))))
      _   <- ref.exclude(DecSet(2, 3, 4, 5)) // only removal of {2, 3} should be logged
    } yield log

    val (store, logRef) = interpreter(prg)(emptyState[Prg])
    val log = fetch(store)(logRef)
    assertResult(List(Diff(Set(2, 3))))(log)
  }

}
