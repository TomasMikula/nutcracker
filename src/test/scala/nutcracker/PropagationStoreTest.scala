package nutcracker

import nutcracker.data.{DecSet, Promise}
import nutcracker.data.DecSet._
import nutcracker.data.listLog._
import nutcracker.ops.Ops._
import nutcracker.toolkit.{CellCycle, FinalizerId, ObserverId, PropagationStore, Token}
import nutcracker.util.Lst
import org.scalatest.funsuite.AnyFunSuite
import scala.annotation.tailrec
import scalaz.Monad
import scalaz.std.anyVal._
import scalaz.syntax.monad._

class PropagationTest extends AnyFunSuite {
  import nutcracker.toolkit.PropBranchToolkit.instance._

  val P = propagationApi

  test("diff accumulation") {
    val prg = for {
      ref <- oneOf(1, 2, 3, 4, 5)
      log <- newLog[Removed[Int]]()
      _   <- P.observe(ref).by(_ => P.sleep(P.continually((s: DecSet[Int], d: Removed[Int]) => log.write(d))))
      _   <- ref.exclude(DecSet(1, 2))
      _   <- ref.exclude(DecSet(4, 5))
    } yield log

    val (store, logRef) = interpret(prg, empty)
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

    val (store1, (ref, logRef)) = interpret(prg1, empty)
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

    val (store, logRef) = interpret(prg, empty)
    val log = fetch(logRef, store)
    assertResult(List(Removed(Set(2, 3))))(log)
  }
}

class PropagationStoreTest extends AnyFunSuite {
  import scalaz.syntax.traverse._

  case class Prg[A](run: PropagationStore[Prg] => (PropagationStore[Prg], A))
  implicit val prgMonad: Monad[Prg] = new Monad[Prg] {
    override def point[A](a: => A): Prg[A] = Prg(s => (s, a))
    override def bind[A, B](fa: Prg[A])(f: (A) => Prg[B]): Prg[B] =
      Prg(s => fa.run(s) match { case (s, a) => f(a).run(s) })
  }

  private def unconsLoop(s: PropagationStore[Prg]): PropagationStore[Prg] =
    interpretAll(s, Lst.empty)

  @tailrec
  private def interpretAll(s: PropagationStore[Prg], l: Lst[Prg[Unit]]): PropagationStore[Prg] =
    l.uncons match {
      case Some((p, ps)) => interpretAll(p.run(s)._1, ps)
      case None => s
    }

  test("resume respects cycle") {

    var setupCalled = 0
    var observed = false

    // create an auto cell
    val s0 = PropagationStore.empty[Prg]
    val (s1, ref) = s0.newAutoCell[Promise[Int]]((ref, cyc) => { setupCalled += 1; ().point[Prg] })
    val s2 = unconsLoop(s1)

    val holdCallbacksRun = collection.mutable.Buffer[Int]()
    val tokens = collection.mutable.Buffer[Token[Promise[Int]]]()

    def hold(s: PropagationStore[Prg], i: Int): (PropagationStore[Prg], CellCycle[Promise[Int]], ObserverId) = {
      val (s1, cyc, oid, ks) = s.hold(ref)((pi, cyc, tok, oid) => {
        holdCallbacksRun.append(i)
        tokens.append(tok)
        ().point[Prg]
      })
      (interpretAll(s1, ks), cyc, oid)
    }

    def resume(s: PropagationStore[Prg], cyc: CellCycle[Promise[Int]], tok: Token[Promise[Int]]): PropagationStore[Prg] = {
      val (s1, ks, becameDirty) = s.resume[Promise[Int], Promise.IDelta[Int, *, *], Promise[Int]](ref, cyc, tok, s.fire[Promise[Int]](Prg(s => { observed = true; (s, ()) })))
      val (s2, ks2) = if(becameDirty) s.execTriggers(ref, cyc) else (s1, ks)
      interpretAll(s2, ks2)
    }

    // observe
    val (s3, cycle1, oid1) = hold(s2, 1)

    assertResult(1)(setupCalled)
    assertResult(0)(holdCallbacksRun.size)

    // supply value
    val s4 = interpretAll(s3, Lst.singleton(Prg(s => s.supply(ref)(cycle1, Promise.empty[Int])).flatMap(_.sequence_)))

    // check that we now have a token, but observer was not triggered yet
    assertResult(1)(tokens.size)
    assertResult(1)(holdCallbacksRun.size)
    assertResult(1)(holdCallbacksRun(0))
    assertResult(false)(observed)

    // remove observer
    val (s5, ks) = s4.rmObserver(ref, cycle1, oid1)
    val s6 = interpretAll(s5, ks)

    // observe again, starts new cell cycle
    val (s7, cycle2, oid2) = hold(s6, 2)

    assertResult(2)(setupCalled)

    // supply value
    val s9 = interpretAll(s7, Lst.singleton(Prg(s => s.supply(ref)(cycle2, Promise.empty[Int])).flatMap(_.sequence_)))

    assertResult(2)(holdCallbacksRun.size)
    assertResult(2)(holdCallbacksRun(1))
    assertResult(2)(tokens.size)
    assertResult(false)(observed)

    // resume the previous (already removed) observer
    val s10 = resume(s9, cycle1, tokens(0))

    // check that we didn't accidentally resume the second observer
    assertResult(false)(observed)

    // resume the current observer
    val s11 = resume(s10, cycle2, tokens(1))

    // check that the observer was finally triggered
    assertResult(true)(observed)
  }

  test("exclUpdate/supply/addFinalizer/removeFinalizer on absent cell") {
    val s0 = PropagationStore.empty[Prg]

    // create cell
    val (s1, ref) = s0.newAutoCell[Promise[Int]]((ref, cyc) => Prg(s => s.supply(ref)(cyc, Promise.completed(42))).flatMap(_.sequence_))

    // observe cell to trigger setup
    val (ks, s2, Some((cycle, oid))) = s1.observeOnce[Promise[Int]](ref, pi => ().point[Prg])
    val s3 = interpretAll(s2, ks)

    // check that cell has already been cleaned up, because once-observer should have fired
    assertResult(None)(s3.tryFetch(ref))

    // these should be no-ops, but cause no error
    s3.exclUpdate[Promise[Int], Promise.Update[Int], Promise.IDelta[Int, *, *]](ref, cycle, Promise.completed(77))
    s3.supply(ref)(cycle, Promise.completed(77))
    s3.addFinalizer(ref, cycle, Subscription())
    s3.removeFinalizer(ref, cycle, FinalizerId.zero)
  }

  test("resume/rmObserver on absent cell") {
    val s0 = PropagationStore.empty[Prg]

    // create cell
    val (s1, ref) = s0.newAutoCell[Promise[Int]]((ref, cyc) => Prg(s => s.supply(ref)(cyc, Promise.completed(42))).flatMap(_.sequence_))

    // hold and snoop the token
    var token: Option[Token[Promise[Int]]] = None
    val (s2, cycle, oid, ks1) = s1.hold(ref)((pi, cyc, tok, oid) => { token = Some(tok); ().point[Prg] })
    val s3 = interpretAll(s2, ks1)
    assert(token.isDefined)

    // remove observer, check that cell has been cleaned up
    val (s4, ks2) = s3.rmObserver(ref, cycle, oid)
    val s5 = interpretAll(s4, ks2)
    assert(s5.tryFetch(ref).isEmpty)

    // these should be no-ops, but cause no error
    s5.rmObserver(ref, cycle, oid)
    s5.resume[Promise[Int], Promise.IDelta[Int, *, *], Promise[Int]](ref, cycle, token.get, s5.fire[Promise[Int]](().point[Prg]))
  }
}

class OnDemandPropagationTest extends AnyFunSuite {
  import nutcracker.toolkit.OnDemandPropagationToolkit.instance._

  private val P = propagationApi

  test("auto-cell setup/cleanup") {
    val setupCalled = collection.mutable.Buffer[Unit]()

    // create simple cell
    val prg0 = P.newCell[Promise[Int]](Promise.empty[Int])
    val (store0, ref0) = interpret0(prg0)

    // create auto-cell that mirrors the simple cell
    val observed0 = collection.mutable.Buffer[Promise[Int]]()
    val prg1 = P.newAutoCell[Promise[Int]](k => {
      setupCalled.append(())
      P.observe(ref0).byM[P.ExclRef[Promise[Int]]](pi =>
        k(pi).map(exclRef => (
          P.untilRightSeq((pi: Promise[Int]) => {
            val update = P.exclUpdate(exclRef).by(pi)
            observed0.append(pi)
            if(pi.isConflict) Right(update) else Left(update)
          }).apply(pi),
          exclRef
        ))
      ).run({ case (sub, exclRef) => P.addFinalizer(exclRef, sub).void })
    })
    val (store1, ref) = interpret(prg1, store0)

    // check that ref0 observer hasn't been triggered yet
    assertResult(0)(observed0.size)

    // check that the cell doesn't have any value yet
    val value1 = fetch(ref, store1)
    assertResult(None)(value1)

    // add an observer
    val observed = collection.mutable.Buffer[Promise[Int]]()
    val prg2 = P.observe(ref).threshold(p => {
      observed.append(p)
      if(p.isCompleted) Some(prgMonad.pure(())) else None
    })
    val (store2, _) = interpret(prg2, store1)

    // check that setup was called
    assertResult(1)(setupCalled.size)

    // check that value is present
    val value2 = fetch(ref, store2)
    assertResult(Some(Promise.empty[Int]))(value2)

    // check that value was observed by both observers
    assertResult(1)(observed0.size)
    assertResult(Promise.empty[Int])(observed0(0))
    assertResult(1)(observed.size)
    assertResult(Promise.empty[Int])(observed(0))

    // complete
    val prg3 = P.update(ref0).by(Promise.completed(42))
    val (store3, _) = interpret(prg3, store2)

    // check that value was observed by both observers
    assertResult(2)(observed0.size)
    assertResult(Promise.completed(42))(observed0(1))
    assertResult(2)(observed.size)
    assertResult(Promise.completed(42))(observed(1))

    // check that cell was garbage collected
    val value3 = fetch(ref, store3)
    assertResult(None)(value3)

    // check that ref0's observer has been removed, thus didn't observe the new value
    val prg4 = P.update(ref0).by(Promise.completed(7))
    val (store4, _) = interpret(prg4, store3)
    assertResult(2)(observed0.size)
  }

  test("unsubscribe bound to a cell cycle") {
    val prg1 = P.newAutoCell[Promise[Int]](k => k(Promise.completed(1)).void)
    val (store1, ref) = interpret0(prg1)

    // add observer
    val prg2 = P.observe(ref).by(P.continually(pi => ().point[Prg]))
    val (store2, sub) = interpret(prg2, store1)

    // remove observer
    val store3 = interpretAll(sub.unsubscribe, store2)
    assertResult(None)(fetch(ref, store3))

    // add another observer
    val prg3 = P.observe(ref).by(P.threshold(pi => None))
    val (store4, _) = interpret(prg3, store3)
    assertResult(Some(Promise.completed(1)))(fetch(ref, store4))

    // remove the first observer again
    val store5 = interpretAll(sub.unsubscribe, store4)

    // check that it didn't accidentally remove the second observer and cleanup the cell
    assertResult(Some(Promise.completed(1)))(fetch(ref, store5))
  }
}