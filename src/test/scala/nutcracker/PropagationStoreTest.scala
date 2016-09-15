package nutcracker

import org.scalatest.FunSuite
import nutcracker.log._
import nutcracker.PropagationLang._
import nutcracker.Trigger._

class PropagationStoreTest extends FunSuite {

  test("diff accumulation") {
    val prg = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      log <- newLog[Diff[Set[Int]]]
      _   <- domTriggerF(ref)(s => (None, Some((s: DecSet[Int], d: Diff[Set[Int]]) => fireReload(log.write(d)))))
      _   <- remove(ref, DecSet(1, 2))
      _   <- remove(ref, DecSet(4, 5))
    } yield log

    val (store, logRef) = PropagationStore.interpreter.freeInstance.apply(prg)(PropagationStore.emptyF[PropagationLang])
    val log = store.fetch(logRef)
    assertResult(List(Diff(Set(1, 2, 4, 5))))(log)
  }

  test("delta is cleared after being handled") {
    val prg1 = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      log <- newLog[Diff[Set[Int]]]
      _   <- domTriggerF(ref)(s => (None, Some((s: DecSet[Int], d: Diff[Set[Int]]) => fireReload(log.write(d)))))
      _   <- remove(ref, DecSet(1, 2))
    } yield (ref, log)

    val interpreter = PropagationStore.interpreter.freeInstance

    val (store1, (ref, logRef)) = interpreter.apply(prg1)(PropagationStore.emptyF[PropagationLang])
    val log1 = store1.fetch(logRef)
    assertResult(List(Diff(Set(1, 2))))(log1)

    val prg2 = remove(ref, DecSet(4, 5))
    val store2 = interpreter(prg2)(store1)._1
    val log2 = store2.fetch(logRef)
    assertResult(List(Diff(Set(4, 5)), Diff(Set(1, 2))))(log2)
  }

  test("delta is published when registering new trigger") {
    val prg = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      _   <- remove(ref, DecSet(4, 5, 6, 7)) // should not be logged
      log <- newLog[Diff[Set[Int]]]
      _   <- domTriggerF(ref)(s => (None, Some((s: DecSet[Int], d: Diff[Set[Int]]) => fireReload(log.write(d)))))
      _   <- remove(ref, DecSet(2, 3, 4, 5)) // only removal of {2, 3} should be logged
    } yield log

    val (store, logRef) = PropagationStore.interpreter.freeInstance.apply(prg)(PropagationStore.emptyF[PropagationLang])
    val log = store.fetch(logRef)
    assertResult(List(Diff(Set(2, 3))))(log)
  }

}
