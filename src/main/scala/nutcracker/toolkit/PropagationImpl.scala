package nutcracker.toolkit

import nutcracker.{IDom, OnDemandPropagation, SeqPreHandler, SeqTrigger, Subscription}
import nutcracker.util.{FreeK, HOrderK, Inject, KMap, Lst, MonadTellState, ShowK, StateInterpreter, StratifiedMonoidAggregator}
import nutcracker.util.ops._
import scala.language.existentials
import scalaz.syntax.equal._
import scalaz.syntax.monoid._
import scalaz.{-\/, Bind, Equal, Lens, Monad, Ordering, Show, \/-}

private[nutcracker] object PropagationImpl extends PersistentOnDemandPropagationModule with FreeOnDemandPropagationToolkit { self =>
  type VarK[K[_], A] = SimpleCellId[K, A]
  type ValK[K[_], A] = CellId[K, A]
  type Lang[K[_], A] = PropagationLang[K, A]
  type StateK[K[_]] = PropagationStore[K]

  override def varOrderK[K[_]]: HOrderK[VarK[K, *]] = SimpleCellId.orderKInstance
  override def varShowK[K[_]]: ShowK[VarK[K, *]] = SimpleCellId.showKInstance
  override def valOrderK[K[_]]: HOrderK[ValK[K, *]] = CellId.orderKInstance
  override def valShowK[K[_]]: ShowK[ValK[K, *]] = CellId.showKInstance

  override def readOnlyK[K[_], A](ref: SimpleCellId[K, A]): CellId[K, A] = ref
  override def prgMonad: Monad[FreeK[Lang, *]] = implicitly
  override implicit def freePropagation[F[_[_], _]](implicit inj: Inject[Lang[FreeK[F, *], *], F[FreeK[F, *], *]]): OnDemandPropagation[FreeK[F, *], VarK[FreeK[F, *], *], ValK[FreeK[F, *], *]] =
    PropagationLang.freePropagation[F]

  override val propagationApi: OnDemandPropagation[Prg, Var, Val] =
    PropagationLang.freePropagation[Lang]

  override def emptyK[K[_]]: PropagationStore[K] =
    PropagationStore.empty[K]

  override val stepInterpreter =
    stepInterpreterK(Lens.lensId[State])

  override def stepInterpreterK[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, *], S] =
    new StateInterpreter[K, Lang[K, *], S] {
      import PropagationLang._

      private def execTriggers[A](ref: SimpleCellId[K, A]): PropagationLang[K, Unit] =
        PropagationLang.execTriggers[K, A](ref)

      private def execTriggersAuto[A](ref: AutoCellId[K, A], cycle: CellCycle[A]): PropagationLang[K, Unit] =
        PropagationLang.ExecTriggersAuto(ref, cycle)

      override def apply[M[_], W, A](p: PropagationLang[K, A])(implicit M: MonadTellState[M, W, S], W: StratifiedMonoidAggregator[W, Lst[K[Unit]]], inj: Inject[PropagationLang[K, *], K], K: Bind[K]): M[A] =
        M.writerState[A](s0 => {
          val s = lens.get(s0)
          p match {
            case upd: Update[k, d, u, δ] =>
              val (s1, becameDirty) = s.update[upd.dom.Domain, upd.dom.Update, upd.dom.IDelta](upd.ref, upd.u)(upd.dom)
              val w = if(becameDirty) Lst.singleton(inj(execTriggers(upd.ref))) at 1 else W.zero
              (w, s0 set s1, ())
            case exc: ExclUpdate[_, a, u, δ] =>
              val (s1, becameDirty) = s.exclUpdate[exc.dom.Domain, exc.dom.Update, exc.dom.IDelta](exc.ref, exc.cycle, exc.u)(exc.dom)
              val w = if(becameDirty) Lst.singleton(inj(execTriggersAuto(exc.ref, exc.cycle))) at 1 else W.zero
              (w, s0 set s1, ())

            case ext: ExecTriggers[K, _] =>
              val (s1, ks) = s.execTriggers(ext.ref)
              (ks at 0, s0 set s1, ())
            case eta: ExecTriggersAuto[K, _] =>
              val (s1, ks) = s.execTriggers(eta.ref, eta.cycle)
              (ks at 0, s0 set s1, ())

            case obs: Observe[k, d, u, δ] =>
              val (ks, s1, oid) = s.observe[obs.dom.Domain, obs.dom.Update, obs.dom.IDelta](obs.ref, obs.f)(obs.dom)
              (ks at 0, s0 set s1, oid)
            case oba: ObserveAuto[k, d, u, δ] =>
              val (ks, s1, oid) = s.observe[oba.dom.Domain, oba.dom.Update, oba.dom.IDelta](oba.ref, oba.f)(oba.dom)
              (ks at 0, s0 set s1, oid)

            case h: Hold[K, d] =>
              val (s1, oid, ks) = s.hold(h.ref)(h.f)
              (ks at 0, s0 set s1, oid)
            case ha: HoldAuto[K, d] =>
              val (s1, cycle, oid, ks) = s.hold(ha.ref)(ha.f) // linter:ignore UndesirableTypeInference
              (ks at 0, s0 set s1, oid)
            case res: Resume[k, d, δ, d0] =>
              val (s1, ks, becameDirty) = s.resume[res.Domain, res.Delta, res.Arg](res.ref, res.token, res.trigger)
              val w = if(becameDirty) (ks at 0) |+| (Lst.singleton(inj(execTriggers(res.ref))) at 1) else (ks at 0)
              (w, s0 set s1, ())
            case res: ResumeAuto[k, d, δ, d0] =>
              val (s1, ks, becameDirty) = s.resume[res.Domain, res.Delta, res.Arg](res.ref, res.cycle, res.token, res.trigger)
              val w = if(becameDirty) (ks at 0) |+| (Lst.singleton(inj(execTriggersAuto(res.ref, res.cycle))) at 1) else (ks at 0)
              (w, s0 set s1, ())

            case nc: NewCell[K, d, u, δ] =>
              val (s1, ref) = s.newCell(nc.d)(nc.dom)
              (W.zero, s0 set s1, ref)
            case nac: NewAutoCell[K, _] =>
              val (s1, ref) = s.newAutoCell(nac.setup) // linter:ignore UndesirableTypeInference
              (W.zero, s0 set s1, ref)

            case sup: Supply[K, d] =>
              val (s1, ks) = s.supply(sup.ref)(sup.cycle, sup.value)
              (ks at 0, s0 set s1, ())

            case rmo: RmObserver[K, d] =>
              val (s1, ks) = s.rmObserver(rmo.ref, rmo.oid)
              (ks at 0, s0 set s1, ())
            case rao: RmAutoObserver[K, d] =>
              val (s1, ks) = s.rmObserver(rao.ref, rao.cycle, rao.oid)
              (ks at 0, s0 set s1, ())

            case add: AddFinalizer[K, _] =>
              val (ks, s1, fid) = s.addFinalizer(add.ref, add.cycle, add.value)
              (ks at 0, s0 set s1, fid)
            case rmf: RemoveFinalizer[K, _] =>
              val s1 = s.removeFinalizer(rmf.ref, rmf.cycle, rmf.id)
              (W.zero, s0 set s1, ())
          }
        })
    }

  override def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] =
    s.tryFetch(ref)

  override def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A =
    s.fetch(ref)

  override def stashable: StashOnDemandPropagationModule.AuxL[self.VarK, self.ValK, self.Lang] =
    new OnDemandPropagationListModule[self.VarK, self.ValK, self.Lang, self.StateK](this)
}


private[nutcracker] case class PropagationStore[K[_]] private(
  lastCellId: CellIdCounter,
  lastCellCycle: CellCycle[_],
  simpleCells: KMap[SimpleCellId[K, *], SimpleCell[K, *]],
  autoCells: KMap[AutoCellId[K, *], OnDemandCell[K, *]]
) {
  type CellIncarnationId[A] = Cell.IncarnationId[K, A]
  type Tok[D, D0] = (CellIncarnationId[D], Token[D0])
  type PreHandler[D, Δ[_, _]] = SeqPreHandler[Tok[D, *], K, D, Δ]
  type Trigger[D, Δ[_, _], D0] = SeqTrigger[Tok[D, *], K, D, Δ, D0]

  // helpers for testing
  def fire[D](k: K[Unit])(implicit dom: IDom[D]): Trigger[D, dom.IDelta, D] = SeqTrigger.Fire[Tok[D, *], K, D, dom.IDelta, D](k)
  def once[D](f: D => K[Unit])(implicit dom: IDom[D]): PreHandler[D, dom.IDelta] = SeqPreHandler[Tok[D, *], K, D, dom.IDelta](d => fire[D](f(d)))

  def newCell[D](d: D)(implicit dom: IDom[D]): (PropagationStore[K], SimpleCellId[K, D]) = {
    val ref = lastCellId.inc[K, D]
    val simpleCells1 = simpleCells.put(ref)(SimpleCell.init(d))
    (copy(lastCellId = ref.counter, simpleCells = simpleCells1), ref)
  }

  def newAutoCell[D](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit]): (PropagationStore[K], AutoCellId[K, D]) = {
    val ref = lastCellId.inc[K, D](setup)
    (copy(lastCellId = ref.counter), ref)
  }

  def tryFetch[D](ref: CellId[K, D]): Option[D] = ref match {
    case r @ SimpleCellId(_) => Some(simpleCells(r).value)
    case r @ AutoCellId(_, _) => autoCells.get(r).flatMap(_.getValue)
  }

  // unsafe, should only be allowed on simple cells
  def fetch[D](ref: CellId[K, D]): D = tryFetch(ref).get

  def update[D, U, Δ[_, _]](ref: SimpleCellId[K, D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Boolean) =
    simpleCells(ref).infer.update(u) match {
      case CellUpdated(cell, becameDirty) =>
        val domains1 = simpleCells.put(ref)(cell)
        (copy(simpleCells = domains1), becameDirty)
      case CellUnchanged =>
        (this, false)
    }

  def exclUpdate[D, U, Δ[_, _]](ref: AutoCellId[K, D], cycle: CellCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Boolean) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        cell0.infer.exclUpdate(u) match {
          case CellUpdated(cell, becameDirty) =>
            val autoCells1 = autoCells.put(ref)(cell)
            (copy(autoCells = autoCells1), becameDirty)
          case CellUnchanged =>
            (this, false)
        }
      case _ =>
        (this, false)
    }

  def observe[D, U, Δ[_, _]](ref: SimpleCellId[K, D], f: PreHandler[D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): (Lst[K[Unit]], PropagationStore[K], Option[ObserverId]) = {
    val (cell, oid, ks) = simpleCells(ref).infer.observe(-\/(ref), f)
    (ks, copy(simpleCells = simpleCells.put(ref)(cell)), oid)
  }

  // convenience method for testing
  def observeOnce[D](ref: AutoCellId[K, D], f: D => K[Unit])(implicit dom: IDom[D]): (Lst[K[Unit]], PropagationStore[K], Option[(CellCycle[D], ObserverId)]) =
    observe[D, dom.Update, dom.IDelta](ref, once(f))(dom)

  def observe[D, U, Δ[_, _]](ref: AutoCellId[K, D], f: PreHandler[D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): (Lst[K[Unit]], PropagationStore[K], Option[(CellCycle[D], ObserverId)]) =
    autoCells.get(ref) match {
      case Some(cell0) =>
        val (cell, oid, ks) = cell0.infer.observe(\/-((ref, cell0.cycle)), f)
        (ks, copy(autoCells = autoCells.put(ref)(cell)), oid.map((cell.cycle, _)))
      case None =>
        val newCycle = lastCellCycle.inc[D]
        val (cell, obsId) = InitializingCell.init[K, D, U, Δ](newCycle, f)
        (Lst.singleton(ref.setup(newCycle)), copy(autoCells = autoCells.put(ref)(cell), lastCellCycle = newCycle), Some((newCycle, obsId)))
    }

  def hold[D](ref: SimpleCellId[K, D])(f: (D, Token[D], ObserverId) => K[Unit]): (PropagationStore[K], ObserverId, Lst[K[Unit]]) = {
    val (cell, obsId, ks) = simpleCells(ref).hold(f)
    (copy(simpleCells = simpleCells.put(ref)(cell)), obsId, ks)
  }

  def hold[D](ref: AutoCellId[K, D])(f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): (PropagationStore[K], CellCycle[D], ObserverId, Lst[K[Unit]]) = {
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

  def supply[D](ref: AutoCellId[K, D])(cycle: CellCycle[D], value: D): (PropagationStore[K], Lst[K[Unit]]) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.supply(\/-((ref, cycle)), value)
        cell match {
          case Some(cell) => (copy(autoCells = autoCells.put(ref)(cell)), ks)
          case None       => (copy(autoCells = autoCells - ref),          ks)
        }
      case _ =>
        (this, Lst.empty)
    }

  def resume[D, Δ[_, _], D0 <: D](ref: SimpleCellId[K, D], token: Token[D0], trigger: Trigger[D, Δ, D0]): (PropagationStore[K], Lst[K[Unit]], Boolean) = {
    val (cell, ks, becameDirty) = simpleCells(ref).asInstanceOf[SimpleCell.AuxD[K, D, Δ]].resume(-\/(ref), token, trigger)
    (copy(simpleCells = simpleCells.put(ref)(cell)), ks, becameDirty)
  }

  def resume[D, Δ[_, _], D0 <: D](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[D0], trigger: Trigger[D, Δ, D0]): (PropagationStore[K], Lst[K[Unit]], Boolean) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.asInstanceOf[OnDemandCell.AuxD[K, D, Δ]].resume(\/-((ref, cycle)), token, trigger)
        cell match {
          case Some((cell, becameDirty)) =>
            (copy(autoCells = autoCells.put(ref)(cell)), ks, becameDirty)
          case None =>
            (copy(autoCells = autoCells - ref), ks, false)
        }
      case _ =>
        (this, Lst.empty, false)
    }

  def rmObserver[D](ref: SimpleCellId[K, D], oid: ObserverId): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = simpleCells(ref).rmObserver(oid)
    (copy(simpleCells = simpleCells.put(ref)(cell)), ks)
  }

  def rmObserver[D](ref: AutoCellId[K, D], cycle: CellCycle[D], oid: ObserverId): (PropagationStore[K], Lst[K[Unit]]) =
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

  def addFinalizer[A](ref: AutoCellId[K, A], cycle: CellCycle[A], sub: Subscription[K]): (Lst[K[Unit]], PropagationStore[K], Option[FinalizerId]) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (ks, cell, fid) = cell0.addFinalizer(sub)
        (ks, copy(autoCells = autoCells.put(ref)(cell)), Some(fid))
      case _ =>
        (sub.unsubscribe, this, None)
    }

  def removeFinalizer[A](ref: AutoCellId[K, A], cycle: CellCycle[A], fid: FinalizerId): PropagationStore[K] =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val cell = cell0.removeFinalizer(fid)
        copy(autoCells = autoCells.put(ref)(cell))
      case _ =>
        this
    }

  def execTriggers[A](r: SimpleCellId[K, A]): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = simpleCells(r).triggerPendingObservers(-\/(r))
    (copy(simpleCells = simpleCells.put(r)(cell)), ks)
  }

  def execTriggers[A](r: AutoCellId[K, A], cycle: CellCycle[A]): (PropagationStore[K], Lst[K[Unit]]) = {
    autoCells.get(r) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.triggerPendingObservers(\/-((r, cell0.cycle)))
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
    simpleCells = KMap[SimpleCellId[K, *], SimpleCell[K, *]](),
    autoCells = KMap[AutoCellId[K, *], OnDemandCell[K, *]]()
  )
}

private[nutcracker] final case class CellIdCounter(lastId: Long) extends AnyVal {
  def inc[K[_], A]: SimpleCellId[K, A] = SimpleCellId[K, A](lastId + 1)
  def inc[K[_], A](setup: (AutoCellId[K, A], CellCycle[A]) => K[Unit]): AutoCellId[K, A] = AutoCellId[K, A](lastId + 1, setup)
}

private[nutcracker] object CellIdCounter {
  def zero: CellIdCounter = CellIdCounter(0L)
}

private[nutcracker] sealed abstract class CellId[K[_], A] {
  type Domain = A
  type Update
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
  type Aux[K[_], A, U, Δ[_, _]] = CellId[K, A] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  def order[K[_], A, B](fa: CellId[K, A], fb: CellId[K, B]): Ordering =
    if(fa.domainId < fb.domainId) Ordering.LT
    else if(fa.domainId == fb.domainId) Ordering.EQ
    else Ordering.GT

  implicit def orderKInstance[K[_]]: HOrderK[CellId[K, *]] = new HOrderK[CellId[K, *]] {
    override def hOrderK[A, B](fa: CellId[K, A], fb: CellId[K, B]): Ordering =
      CellId.order(fa, fb)
  }

  implicit def showKInstance[K[_]]: ShowK[CellId[K, *]] = new ShowK[CellId[K, *]] {
    def shows[A](ref: CellId[K, A]): String = s"cell${ref.domainId}"
  }
}

private[nutcracker] final case class SimpleCellId[K[_], D](domainId: Long) extends CellId[K, D] {
  override def aux: SimpleCellId.Aux[K, Domain, Update, Delta] = this
}

private[nutcracker] object SimpleCellId {
  type Aux[K[_], D, U, Δ[_, _]] = SimpleCellId[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  implicit def orderKInstance[K[_]]: HOrderK[SimpleCellId[K, *]] = new HOrderK[SimpleCellId[K, *]] {
    override def hOrderK[A, B](fa: SimpleCellId[K, A], fb: SimpleCellId[K, B]): Ordering =
      CellId.order(fa, fb)
  }

  implicit def showInstance[K[_], D]: Show[SimpleCellId[K, D]] =
    Show.shows[SimpleCellId[K, D]](ref => s"ref${ref.domainId}")

  implicit def showKInstance[K[_]]: ShowK[SimpleCellId[K, *]] = new ShowK[SimpleCellId[K, *]] {
    def shows[A](ref: SimpleCellId[K, A]): String = s"cell${ref.domainId}"
  }
}

private[nutcracker] final case class AutoCellId[K[_], A](domainId: Long, runSetup: (AutoCellId[K, A], CellCycle[A]) => K[Unit]) extends CellId[K, A] {
  def setup(cycle: CellCycle[A]): K[Unit] = runSetup(this, cycle)
}

private[nutcracker] final class CellCycle[A] private(val value: Long) extends AnyVal {
  def inc[B]: CellCycle[B] = new CellCycle[B](value + 1)
}

private[nutcracker] object CellCycle {
  def zero[A]: CellCycle[A] = new CellCycle(0)

  implicit def equalInstance[A]: Equal[CellCycle[A]] = new Equal[CellCycle[A]] {
    def equal(a1: CellCycle[A], a2: CellCycle[A]): Boolean = a1.value == a2.value
  }
}