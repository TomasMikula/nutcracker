package nutcracker.toolkit

import nutcracker.{IDom, OnDemandPropagation, SeqPreHandler, SeqTrigger, Subscription}
import nutcracker.Propagation.IUpdateRes
import nutcracker.util.{Exists, ExistsOption, ENone, ESome, FreeK, HKMap, HOrderK, Inject, Lst, MonadTellState, ShowK, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops.Ops._
import scala.language.existentials
import scalaz.syntax.equal._
import scalaz.syntax.monoid._
import scalaz.{-\/, Bind, Equal, Lens, Monad, Ordering, Show, \/-}

private[nutcracker] object PropagationImpl extends PersistentOnDemandPropagationModule with FreeOnDemandPropagationToolkit { self =>
  override type VarK[K[_], A] = SimpleCellId[K, [i] =>> A]
  override type ValK[K[_], A] = CellId[K, [i] =>> A]
  override type OutK[K[_], A] = nutcracker.toolkit.Out[[a] =>> SimpleCellId[K, [i] =>> a], A]
  override type Lang[K[_], A] = PropagationLang[K, A]
  override type StateK[K[_]] = PropagationStore[K]

  override def varOrderK[K[_]]: HOrderK[VarK[K, *]] = HOrderK([a, b] => (a: VarK[K, a], b: VarK[K, b]) => SimpleCellId.order(a, b))
  override def varShowK[K[_]]: ShowK[VarK[K, *]]    = ShowK([a] => (a: VarK[K, a]) => SimpleCellId.show(a))
  override def valOrderK[K[_]]: HOrderK[ValK[K, *]] = HOrderK([a, b] => (a: ValK[K, a], b: ValK[K, b]) => CellId.order(a, b))
  override def valShowK[K[_]]: ShowK[ValK[K, *]]    = ShowK([a] => (a: ValK[K, a]) => CellId.show(a))

  override def readOnlyK[K[_], A](ref: VarK[K, A]): ValK[K, A] = ref
  override def prgMonad: Monad[FreeK[Lang, *]] = FreeK.freeKMonad

  override implicit def freePropagation[F[_[_], _]](implicit
    inj: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]],
  ): OnDemandPropagation.Aux[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *], OutK[FreeK[F, *], *]] =
    PropagationLang.freePropagation[F](inj)

  override val propagationApi: FreePropagation[Lang] with OnDemandPropagation.Aux[Prg, Var, Val, Out] =
    PropagationLang.freePropagation[Lang]

  override def emptyK[K[_]]: PropagationStore[K] =
    PropagationStore.empty[K]

  override val stepInterpreter =
    stepInterpreterK(Lens.lensId[State])

  override def stepInterpreterK[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] =
    new StateInterpreter[K, Lang[K, *], S] {
      import PropagationLang._

      private def execTriggers[D[_]](ref: SimpleCellId[K, D]): PropagationLang[K, Unit] =
        PropagationLang.execTriggers[K, D](ref)

      private def execTriggersAuto[A[_]](ref: AutoCellId[K, A], cycle: CellCycle[A]): PropagationLang[K, Unit] =
        PropagationLang.ExecTriggersAuto(ref, cycle)

      override def apply[M[_], W, A](p: PropagationLang[K, A])(implicit
        M: MonadTellState[M, W, S],
        W: StratifiedMonoidAggregator[W, Lst[K[Unit]]],
        inj: Inject[PropagationLang[K, *], K],
        K: Bind[K],
      ): M[A] = {
        def resumeSimple[D[_]](cellId: SimpleCellId[K, D]): [δ[_, _], i] => (Token[i], SeqTrigger[K, D, δ, i]) => K[Unit] =
          [δ[_, _], i] => (tk: Token[i], tr: SeqTrigger[K, D, δ, i]) =>
            inj(PropagationLang.resume(cellId, tk, tr))
        def resumeSimple_[D[_], Δ[_, _]](cellId: SimpleCellId[K, D]): [i] => (Token[i], SeqTrigger[K, D, Δ, i]) => K[Unit] =
          [i] => (tk: Token[i], tr: SeqTrigger[K, D, Δ, i]) =>
            resumeSimple(cellId)(tk, tr)

        def resumeAuto[D[_]](cellId: AutoCellId[K, D], cycle: CellCycle[D]): [δ[_, _], i] => (Token[i], SeqTrigger[K, D, δ, i]) => K[Unit] =
          [δ[_, _], i] => (tk: Token[i], tr: SeqTrigger[K, D, δ, i]) =>
            inj(PropagationLang.resumeAuto(cellId, cycle, tk, tr))

        def resumeAuto_[D[_], Δ[_, _]](cellId: AutoCellId[K, D]): [i] => (CellCycle[D], Token[i], SeqTrigger[K, D, Δ, i]) => K[Unit] =
          [i] => (cycle: CellCycle[D], tk: Token[i], tr: SeqTrigger[K, D, Δ, i]) =>
            resumeAuto(cellId, cycle)(tk, tr)

        M.writerState[A](s0 => {
          val s = lens.get(s0)
          p match {
            case upd: Update[k, d, u, ch, j] =>
              val (s1, res, becameDirty) = s.update[d, u, j](upd.ref, upd.u)(upd.dom)
              val w = if(becameDirty) Lst.singleton(inj(execTriggers(upd.ref))) at 1 else W.zero
              (w, s0 set s1, res)
            case exc: ExclUpdate[k, d, u, δ, j] =>
              val (s1, becameDirty) = s.exclUpdate[d, u, δ, j](exc.ref, exc.cycle, exc.u)(exc.dom)
              val w = if(becameDirty) Lst.singleton(inj(execTriggersAuto(exc.ref, exc.cycle))) at 1 else W.zero
              (w, s0 set s1, ())

            case ext: ExecTriggers[k, _] =>
              val cellId = ext.ref
              val (s1, ks) = s.execTriggers(cellId, resumeSimple(cellId))
              (ks at 0, s0 set s1, ())
            case eta: ExecTriggersAuto[k, _] =>
              val (s1, ks) = s.execTriggers(eta.ref, eta.cycle, resumeAuto(eta.ref, eta.cycle))
              (ks at 0, s0 set s1, ())

            case obs: Observe[k, d, u, δ] =>
              val (ks, s1, oid) = s.observe[d](obs.ref)(using obs.dom)(obs.f, resumeSimple_[d, obs.dom.IDelta](obs.ref))
              (ks at 0, s0 set s1, oid)
            case oba: ObserveAuto[k, d, u, δ] =>
              val (ks, s1, oid) = s.observe[d, u, δ](oba.ref, oba.f, resumeAuto_[d, δ](oba.ref))(oba.dom)
              (ks at 0, s0 set s1, oid)

            case h: Hold[k, d] =>
              val (s1, oid, ks) = s.hold(h.ref)(h.f)
              (ks at 0, s0 set s1, oid)
            case ha: HoldAuto[k, d] =>
              val (s1, cycle, oid, ks) = s.hold(ha.ref)(ha.f)
              (ks at 0, s0 set s1, oid)
            case res: Resume[k, d, δ, d0] =>
              def resume[D[_], Δ[_, _], I](s: StateK[K])(ref: SimpleCellId[K, D], token: Token[I], trigger: s.Trigger[D, Δ, I]): (PropagationStore[K], Lst[K[Unit]], Boolean) =
                s.resume[D, Δ, I](ref, token, trigger, resumeSimple(ref)[Δ, I])
              val (s1, ks, becameDirty) =
                resume[res.Domain, res.Delta, res.Idx](s)(res.ref, res.token, res.trigger)
              val w = if(becameDirty) (ks at 0) |+| (Lst.singleton(inj(execTriggers(res.ref))) at 1) else (ks at 0)
              (w, s0 set s1, ())
            case res: ResumeAuto[k, d, δ, d0] =>
              def resume[D[_], Δ[_, _], I](s: StateK[K])(ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[I], trigger: s.Trigger[D, Δ, I]): (PropagationStore[K], Lst[K[Unit]], Boolean) =
                s.resume(ref, cycle, token, trigger, resumeAuto(ref, cycle)[Δ, I])
              val (s1, ks, becameDirty) = resume[res.Domain, res.Delta, res.Idx](s)(res.ref, res.cycle, res.token, res.trigger)
              val w = if(becameDirty) (ks at 0) |+| (Lst.singleton(inj(execTriggersAuto(res.ref, res.cycle))) at 1) else (ks at 0)
              (w, s0 set s1, ())

            case nc: NewCell[k, d, i] =>
              val (s1, ref) = s.newCell(nc.d)(nc.dom)
              (W.zero, s0 set s1, ref)
            case nac: NewAutoCell[k, d] =>
              val (s1, ref) = s.newAutoCell(nac.setup)
              (W.zero, s0 set s1, ref)

            case sup: Supply[k, d, i] =>
              val (s1, ks) = s.supply(sup.ref)(sup.cycle, sup.value, resumeAuto(sup.ref, sup.cycle))
              (ks at 0, s0 set s1, ())

            case rmo: RmObserver[k, d] =>
              val (s1, ks) = s.rmObserver(rmo.ref, rmo.oid)
              (ks at 0, s0 set s1, ())
            case rao: RmAutoObserver[k, d] =>
              val (s1, ks) = s.rmObserver(rao.ref, rao.cycle, rao.oid)
              (ks at 0, s0 set s1, ())

            case add: AddFinalizer[k, _] =>
              val (ks, s1, fid) = s.addFinalizer(add.ref, add.cycle, add.value)
              (ks at 0, s0 set s1, fid)
            case rmf: RemoveFinalizer[k, _] =>
              val s1 = s.removeFinalizer(rmf.ref, rmf.cycle, rmf.id)
              (W.zero, s0 set s1, ())
          }
        })
      }
    }

  override def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] =
    s.tryFetch(ref) match {
      case ESome(a) => Some(a)
      case ENone()  => None
    }

  override def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A =
    s.fetch(ref).value

  override def readOutK[K[_], A](a: OutK[K, A], s: StateK[K]): A =
    a match {
      case Out.Const(a)      => a
      case Out.WrapVar(va)   => fetchK(va, s)
      case Out.Mapped(x, f)  => f(readOutK(x, s))
      case Out.Pair(x, y)    => (readOutK(x, s), readOutK(y, s))
      case Out.FlatMap(x, f) => readOutK(f(readOutK(x, s)), s)
    }

  override def stashable: StashOnDemandPropagationModule.AuxL[self.VarK, self.ValK, self.OutK, self.Lang] =
    new OnDemandPropagationListModule[self.VarK, self.ValK, self.OutK, self.Lang, self.StateK](this)
}


private[nutcracker] case class PropagationStore[K[_]] private(
  lastCellId: CellIdCounter,
  lastCellCycle: CellCycle[_],
  simpleCells: HKMap[[d[_]] =>> SimpleCellId[K, d], [d[_]] =>> SimpleCell[K, d]],
  autoCells: HKMap[[d[_]] =>> AutoCellId[K, d], [d[_]] =>> OnDemandCell[K, d]]
) {
  type PreHandler[D[_], Δ[_, _]] = SeqPreHandler[K, D, Δ]
  type Trigger[D[_], Δ[_, _], I] = SeqTrigger[K, D, Δ, I]

  // helpers for testing
  def fire[D[_], I](k: K[Unit])(implicit dom: IDom[D]): Trigger[D, dom.IDelta, I] = SeqTrigger.Fire[K, D, dom.IDelta, I](k)
  def once[D[_]](f: [i] => D[i] => K[Unit])(implicit dom: IDom[D]): PreHandler[D, dom.IDelta] = SeqPreHandler[K, D, dom.IDelta]([i] => (d: D[i]) => fire[D, i](f[i](d)))

  def newCell[D[_], I](d: D[I])(implicit dom: IDom[D]): (PropagationStore[K], SimpleCellId[K, D]) = {
    val ref = lastCellId.inc[K, D]
    val simpleCells1 = simpleCells.put(ref)(SimpleCell.init(d))
    (copy(lastCellId = ref.counter, simpleCells = simpleCells1), ref)
  }

  def newAutoCell[D[_]](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]): (PropagationStore[K], AutoCellId[K, D]) = {
    val ref = lastCellId.inc[K, D](setup)
    (copy(lastCellId = ref.counter), ref)
  }

  def tryFetch[D[_]](ref: CellId[K, D]): ExistsOption[D] = ref match {
    case r @ SimpleCellId(_)  =>
      ESome(simpleCells(r).value)
    case r @ AutoCellId(_, _) =>
      autoCells.get(r) match {
        case Some(cell) => ExistsOption.fromOption(cell.getValue)
        case None       => ENone()
      }
  }

  // unsafe, should only be allowed on simple cells
  def fetch[D[_]](ref: CellId[K, D]): Exists[D] = tryFetch(ref).unsafeGet

  def update[D[_], U[_], J](ref: SimpleCellId[K, D], u: U[J])(implicit dom: IDom.Aux0[D, U]): (PropagationStore[K], IUpdateRes[D, dom.IChange, J, ?], Boolean) =
    simpleCells(ref).infer.update(u) match {
      case CellUpdated(cell, change, newValue, becameDirty) =>
        val domains1 = simpleCells.put(ref)(cell)
        (copy(simpleCells = domains1), IUpdateRes.Updated(change, newValue), becameDirty)
      case CellUnchanged(value) =>
        (this, IUpdateRes.Unchanged(value), false)
    }

  def exclUpdate[D[_], U[_], Δ[_, _], J](ref: AutoCellId[K, D], cycle: CellCycle[D], u: U[J])(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Boolean) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        cell0.infer.exclUpdate(u) match {
          case CellUpdated(cell, delta, newValue, becameDirty) =>
            val autoCells1 = autoCells.put(ref)(cell)
            (copy(autoCells = autoCells1), becameDirty)
          case CellUnchanged(value) =>
            (this, false)
        }
      case _ =>
        (this, false)
    }

  def observe[D[_]](
    ref: SimpleCellId[K, D],
  )(using
    dom: IDom[D],
  )(
    f: PreHandler[D, dom.IDelta],
    onReconsidered: [i] => (Token[i], Trigger[D, dom.IDelta, i]) => K[Unit],
  ): (Lst[K[Unit]], PropagationStore[K], Option[ObserverId]) = {
    val cell0 = simpleCells(ref).infer
    val (cell, oid, ks) = cell0.observe(-\/(ref), f, onReconsidered[cell0.Idx])
    (ks, copy(simpleCells = simpleCells.put(ref)(cell)), oid)
  }

  // convenience method for testing
  def observeOnce[D[_]](
    ref: AutoCellId[K, D],
    f: [i] => D[i] => K[Unit],
  )(using
    dom: IDom[D],
  )(
    onReconsidered: [i] => (CellCycle[D], Token[i], Trigger[D, dom.IDelta, i]) => K[Unit],
  ): (Lst[K[Unit]], PropagationStore[K], Option[(CellCycle[D], ObserverId)]) =
    observe[D, dom.IUpdate, dom.IDelta](ref, once[D](f), onReconsidered)(dom)

  def observe[D[_], U[_], Δ[_, _]](
    ref: AutoCellId[K, D],
    f: PreHandler[D, Δ],
    onReconsidered: [i] => (CellCycle[D], Token[i], Trigger[D, Δ, i]) => K[Unit],
  )(implicit
    dom: IDom.Aux[D, U, Δ],
  ): (Lst[K[Unit]], PropagationStore[K], Option[(CellCycle[D], ObserverId)]) =
    autoCells.get(ref) match {
      case Some(cell0) =>
        val cell1 = cell0.infer
        val (cell, oid, ks) = cell1.observe(\/-((ref, cell1.cycle)), f, onReconsidered[cell1.Idx](cell1.cycle, _, _))
        (ks, copy(autoCells = autoCells.put(ref)(cell)), oid.map((cell.cycle, _)))
      case None =>
        val newCycle = lastCellCycle.inc[D]
        val (cell, obsId) = InitializingCell.init[K, D, U, Δ](newCycle, f)
        (Lst.singleton(ref.setup(newCycle)), copy(autoCells = autoCells.put(ref)(cell), lastCellCycle = newCycle), Some((newCycle, obsId)))
    }

  def hold[D[_]](ref: SimpleCellId[K, D])(f: [i] => (D[i], Token[i], ObserverId) => K[Unit]): (PropagationStore[K], ObserverId, Lst[K[Unit]]) = {
    val (cell, obsId, ks) = simpleCells(ref).hold(f)
    (copy(simpleCells = simpleCells.put(ref)(cell)), obsId, ks)
  }

  def hold[D[i]](ref: AutoCellId[K, D])(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): (PropagationStore[K], CellCycle[D], ObserverId, Lst[K[Unit]]) = {
    autoCells.get(ref) match {
      case Some(cell0) =>
        val (cell, obsId, ks) = cell0.hold(f)
        (copy(autoCells = autoCells.put(ref)(cell)), cell.cycle, obsId, ks)
      case None =>
        val newCycle = lastCellCycle.inc[D]
        val (cell, obsId) = InitializingCell.init[K, D, ref.Update, ref.Delta](newCycle, f)
        (copy(autoCells = autoCells.put(ref)(cell), lastCellCycle = newCycle), newCycle, obsId, Lst.singleton(ref.setup(newCycle)))
    }
  }

  def supply[D[_], I](ref: AutoCellId[K, D])(
    cycle: CellCycle[D],
    value: D[I],
    onReconsidered: [δ[_, _], i] => (Token[i], Trigger[D, δ, i]) => K[Unit],
  ): (PropagationStore[K], Lst[K[Unit]]) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.supply(\/-((ref, cycle)), value, onReconsidered[cell0.Delta, I])
        cell match {
          case Some(cell) => (copy(autoCells = autoCells.put(ref)(cell)), ks)
          case None       => (copy(autoCells = autoCells - ref),          ks)
        }
      case _ =>
        (this, Lst.empty)
    }

  def resume[D[_], Δ[_, _], I](
    ref: SimpleCellId[K, D],
    token: Token[I],
    trigger: Trigger[D, Δ, I],
    onReconsidered: (Token[I], Trigger[D, Δ, I]) => K[Unit],
  ): (PropagationStore[K], Lst[K[Unit]], Boolean) = {
    val cell0 = simpleCells(ref).asInstanceOf[SimpleCell.AuxD[K, D, Δ]]
    val (cell, ks, becameDirty) = cell0.resume(-\/(ref), token, trigger, onReconsidered)
    (copy(simpleCells = simpleCells.put(ref)(cell)), ks, becameDirty)
  }

  def resume[D[_], Δ[_, _], I](
    ref: AutoCellId[K, D],
    cycle: CellCycle[D],
    token: Token[I],
    trigger: Trigger[D, Δ, I],
    onReconsidered: (Token[I], Trigger[D, Δ, I]) => K[Unit],
  ): (PropagationStore[K], Lst[K[Unit]], Boolean) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) =
          cell0
            .asInstanceOf[OnDemandCell.AuxD[K, D, Δ]]
            .resume(\/-((ref, cycle)), token, trigger, onReconsidered)
        cell match {
          case Some((cell, becameDirty)) =>
            (copy(autoCells = autoCells.put(ref)(cell)), ks, becameDirty)
          case None =>
            (copy(autoCells = autoCells - ref), ks, false)
        }
      case _ =>
        (this, Lst.empty, false)
    }

  def rmObserver[D[_]](ref: SimpleCellId[K, D], oid: ObserverId): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = simpleCells(ref).rmObserver(oid)
    (copy(simpleCells = simpleCells.put(ref)(cell)), ks)
  }

  def rmObserver[D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId): (PropagationStore[K], Lst[K[Unit]]) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.rmObserver(oid)
        cell match {
          case Some(cell) => (copy(autoCells = autoCells.put(ref)(cell)), ks)
          case None       => (copy(autoCells = autoCells - ref),          ks)
        }
      case _ =>
        (this, Lst.empty)
    }

  def addFinalizer[D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], sub: Subscription[K]): (Lst[K[Unit]], PropagationStore[K], Option[FinalizerId]) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (ks, cell, fid) = cell0.addFinalizer(sub)
        (ks, copy(autoCells = autoCells.put(ref)(cell)), Some(fid))
      case _ =>
        (sub.unsubscribe, this, None)
    }

  def removeFinalizer[D[_]](ref: AutoCellId[K, D], cycle: CellCycle[D], fid: FinalizerId): PropagationStore[K] =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val cell = cell0.removeFinalizer(fid)
        copy(autoCells = autoCells.put(ref)(cell))
      case _ =>
        this
    }

  def execTriggers[D[_]](
    r: SimpleCellId[K, D],
    onReconsidered: [δ[_, _], i] => (Token[i], Trigger[D, δ, i]) => K[Unit],
  ): (PropagationStore[K], Lst[K[Unit]]) = {
    val cell0 = simpleCells(r)
    val (cell, ks) = cell0.triggerPendingObservers(-\/(r), onReconsidered[cell0.Delta, cell0.Idx])
    (copy(simpleCells = simpleCells.put(r)(cell)), ks)
  }

  def execTriggers[D[_]](
    r: AutoCellId[K, D],
    cycle: CellCycle[D],
    onReconsidered: [δ[_, _], i] => (Token[i], Trigger[D, δ, i]) => K[Unit],
  ): (PropagationStore[K], Lst[K[Unit]]) = {
    autoCells.get(r) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.triggerPendingObservers(\/-((r, cell0.cycle)), onReconsidered[cell0.Delta, cell0.Idx])
        cell match {
          case Some(cell) =>
            (copy(autoCells = autoCells.put(r)(cell)), ks)
          case None =>
            (copy(autoCells = autoCells - r), ks)
        }
      case _ =>
        (this, Lst.empty)
    }
  }
}

object PropagationStore {
  def empty[K[_]]: PropagationStore[K] = PropagationStore[K](
    lastCellId = CellIdCounter.zero,
    lastCellCycle = CellCycle.zero[Nothing],
    simpleCells = HKMap[[d[_]] =>> SimpleCellId[K, d], [d[_]] =>> SimpleCell[K, d]](),
    autoCells = HKMap[[d[_]] =>> AutoCellId[K, d], [d[_]] =>> OnDemandCell[K, d]]()
  )
}

private[nutcracker] final case class CellIdCounter(lastId: Long) extends AnyVal {
  def inc[K[_], D[_]]: SimpleCellId[K, D] = SimpleCellId[K, D](lastId + 1)
  def inc[K[_], D[_]](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]): AutoCellId[K, D] = AutoCellId[K, D](lastId + 1, setup)
}

private[nutcracker] object CellIdCounter {
  def zero: CellIdCounter = CellIdCounter(0L)
}

private[nutcracker] sealed abstract class CellId[K[_], A[_]] {
  type Domain[I] = A[I]
  type Update[J]
  type Delta[_, _]

  val domainId: Long

  def aux: CellId.Aux[K, Domain, Update, Delta] = this

  def counter: CellIdCounter = CellIdCounter(domainId)

  def fold[B](
    caseSimple: SimpleCellId[K, A] => B,
    caseAuto: AutoCellId[K, A] => B
  ): B = this match {
    case c @ SimpleCellId(_) => caseSimple(c)
    case c @ AutoCellId(_, _) => caseAuto(c)
  }
}

private[nutcracker] object CellId {
  type Aux[K[_], D[_], U[_], Δ[_, _]] = CellId[K, D] { type Update[J] = U[J]; type Delta[I, J] = Δ[I, J] }

  def order[K[_], A[_], B[_]](fa: CellId[K, A], fb: CellId[K, B]): Ordering =
    if(fa.domainId < fb.domainId) Ordering.LT
    else if(fa.domainId == fb.domainId) Ordering.EQ
    else Ordering.GT

  def show[K[_], D[_]](ref: CellId[K, D]): String =
    s"cell${ref.domainId}"
}

private[nutcracker] final case class SimpleCellId[K[_], D[_]](domainId: Long) extends CellId[K, D] {
  override def aux: SimpleCellId.Aux[K, Domain, Update, Delta] = this
}

private[nutcracker] object SimpleCellId {
  type AuxΔ[K[_], D[_], Δ[_, _]] = SimpleCellId[K, D] { type Delta[I, J] = Δ[I, J] }
  type Aux[K[_], D[_], U[_], Δ[_, _]] = SimpleCellId[K, D] { type Update[J] = U[J]; type Delta[I, J] = Δ[I, J] }

  def order[K[_], A[_], B[_]](fa: SimpleCellId[K, A], fb: SimpleCellId[K, B]): Ordering =
    CellId.order(fa, fb)

  def show[K[_], D[_]](ref: SimpleCellId[K, D]): String =
    s"ref${ref.domainId}"
}

private[nutcracker] final case class AutoCellId[K[_], D[_]](domainId: Long, runSetup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]) extends CellId[K, D] {
  def setup(cycle: CellCycle[D]): K[Unit] = runSetup(this, cycle)
}

private[nutcracker] object AutoCellId {
  type AuxΔ[K[_], D[_], Δ[_, _]] = AutoCellId[K, D] { type Delta[I, J] = Δ[I, J] }
}

private[nutcracker] final class CellCycle[D[_]] private(val value: Long) extends AnyVal {
  def inc[E[_]]: CellCycle[E] = new CellCycle[E](value + 1)
}

private[nutcracker] object CellCycle {
  def zero[D[_]]: CellCycle[D] = new CellCycle(0)

  implicit def equalInstance[D[_]]: Equal[CellCycle[D]] = new Equal[CellCycle[D]] {
    def equal(a1: CellCycle[D], a2: CellCycle[D]): Boolean = a1.value == a2.value
  }
}