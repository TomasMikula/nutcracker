package nutcracker

import nutcracker.Dom.Diff
import org.scalatest.FunSuite
import nutcracker.log._
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.FreeK

import scalaz.\/-

class PropagationStoreTest extends FunSuite {

  test("diff accumulation") {
    val prg = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      log <- newLog[CMDelta[Set[Int]]]
      _   <- domTriggerF(ref)(s => (None, Some((s: Set[Int], d: CMDelta[Set[Int]]) => fireReload(log.write(d)))))
      _   <- remove(ref, Set(1, 2))
      _   <- remove(ref, Set(4, 5))
    } yield log

    val (store, logRef) = PropagationStore.interpreter.freeInstance.apply(prg)(PropagationStore.empty[FreeK[PropagationLang, ?]])
    val log = store.fetch(logRef)
    assertResult(List(\/-(Diff(Set(1, 2, 4, 5)))))(log)
  }

}
