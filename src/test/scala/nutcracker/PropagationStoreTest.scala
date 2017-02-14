package nutcracker

import org.scalatest.FunSuite
import nutcracker.log._
import nutcracker.Trigger._
import nutcracker.util.FreeK

class PropagationStoreTest extends FunSuite {
  val Prop = Propagation.module
  import Prop._

  type Prg[A] = FreeK[Prop.Lang, A]

  val P = Propagation[Prg, Ref]
  val V = FinalVars[Prg, Ref]
  import P._
  import V._

  test("diff accumulation") {
    val prg = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      log <- newLog[Prg, Ref, Diff[Set[Int]]]
      _   <- observe(ref).by(s => (None, Some((s: DecSet[Int], d: Diff[Set[Int]]) => fireReload(log.write[Prg](d)))))
      _   <- remove(ref, DecSet(1, 2))
      _   <- remove(ref, DecSet(4, 5))
    } yield log

    val (store, logRef) = interpreter.freeInstance.apply(prg)(emptyF[Prop.Lang])
    val log = fetch(store)(logRef)
    assertResult(List(Diff(Set(1, 2, 4, 5))))(log)
  }

  test("delta is cleared after being handled") {
    val prg1 = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      log <- newLog[Prg, Ref, Diff[Set[Int]]]
      _   <- observe(ref).by(s => (None, Some((s: DecSet[Int], d: Diff[Set[Int]]) => fireReload(log.write[Prg](d)))))
      _   <- remove(ref, DecSet(1, 2))
    } yield (ref, log)

    val interp = interpreter.freeInstance

    val (store1, (ref, logRef)) = interp.apply(prg1)(emptyF[Prop.Lang])
    val log1 = fetch(store1)(logRef)
    assertResult(List(Diff(Set(1, 2))))(log1)

    val prg2 = remove(ref, DecSet(4, 5))
    val store2 = interp(prg2)(store1)._1
    val log2 = fetch(store2)(logRef)
    assertResult(List(Diff(Set(4, 5)), Diff(Set(1, 2))))(log2)
  }

  test("delta is published when registering new trigger") {
    val prg = for {
      ref <- variable[Int].oneOf(1, 2, 3, 4, 5)
      _   <- remove(ref, DecSet(4, 5, 6, 7)) // should not be logged
      log <- newLog[Prg, Ref, Diff[Set[Int]]]
      _   <- observe(ref).by(s => (None, Some((s: DecSet[Int], d: Diff[Set[Int]]) => fireReload(log.write[Prg](d)))))
      _   <- remove(ref, DecSet(2, 3, 4, 5)) // only removal of {2, 3} should be logged
    } yield log

    val (store, logRef) = interpreter.freeInstance.apply(prg)(emptyF[Prop.Lang])
    val log = fetch(store)(logRef)
    assertResult(List(Diff(Set(2, 3))))(log)
  }

}
