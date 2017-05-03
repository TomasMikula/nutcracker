package nutcracker

import nutcracker.util.{FreeK, FreeKT, HOrderK, Index, InjectK, KMap, KMapB, Lst, ShowK, StateInterpreter, Step, Uncons, WriterState, `Forall{(* -> *) -> *}`, ∃}
import scala.annotation.tailrec
import scala.language.existentials
import scalaz.{-\/, Equal, Leibniz, Monad, Ordering, Show, StateT, \/, \/-, ~>}
import scalaz.Id.Id
import scalaz.std.option._
import scalaz.Leibniz.===
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import shapeless.{HList, Nat, Sized}

private[nutcracker] object PropagationImpl extends PersistentOnDemandPropagationModule with OnDemandPropagationBundle { self =>
  type VarK[K[_], A] = SimpleCellId[K, A]
  type ValK[K[_], A] = CellId[K, A]
  type Lang[K[_], A] = PropagationLang[K, A]
  type StateK[K[_]] = PropagationStore[K]

  override def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = SimpleCellId.orderKInstance
  override def varShowK[K[_]]: ShowK[VarK[K, ?]] = SimpleCellId.showKInstance
  override def valOrderK[K[_]]: HOrderK[ValK[K, ?]] = CellId.orderKInstance
  override def valShowK[K[_]]: ShowK[ValK[K, ?]] = CellId.showKInstance

  override implicit def prgMonad: Monad[FreeK[Lang, ?]] = FreeKT.freeKTMonad[Lang, Id]
  override implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): OnDemandPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    PropagationLang.freePropagation[F]

  override val propagationApi: OnDemandPropagation[Prg, Var, Val] =
    PropagationLang.freePropagation[Lang]

  override def emptyK[K[_]]: PropagationStore[K] =
    PropagationStore.empty[K]

  override def interpret[A](p: Prg[A], s: PropagationStore[Prg]): (PropagationStore[Prg], A) =
    interpreter.freeInstance.apply(p).run(s)

  override val interpreter: StateInterpreter[Lang, StateK] =
    new StateInterpreter[PropagationLang, PropagationStore] {

      def step: Step[PropagationLang, PropagationStore] =
        new Step[PropagationLang, PropagationStore] {
          import PropagationLang._
          override def apply[K[_]: Monad, A](p: PropagationLang[K, A]): WriterState[Lst[K[Unit]], PropagationStore[K], A] = WriterState(s =>
            p match {
              case Update(ref, u, dom) =>
                (Lst.empty, s.update[dom.Domain, dom.Update, dom.IDelta](ref, u)(dom), ())
              case Observe(ref, f, dom) =>
                s.observe[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom)
              case ObserveAuto(ref, f, dom) =>
                s.observe[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom)
              case NewCell(d, dom) =>
                val (s1, ref) = s.newCell(d)(dom) // linter:ignore UndesirableTypeInference
                (Lst.empty, s1, ref)

              case Hold(ref, f) =>
                val (s1, oid, ks) = s.hold(ref)(f)
                (ks, s1, oid)
              case HoldAuto(ref, f) =>
                val (s1, cycle, oid, ks) = s.hold(ref)(f) // linter:ignore UndesirableTypeInference
                (ks, s1, oid)
              case res @ Resume(ref, token, trigger) =>
                val (s1, ks) = s.resume[res.Domain, res.Delta, res.Arg](ref, token, trigger)
                (ks, s1, ())
              case res @ ResumeAuto(ref, cycle, token, trigger) =>
                val (s1, ks) = s.resume[res.Domain, res.Delta, res.Arg](ref, cycle, token, trigger)
                (ks, s1, ())
              case RmObserver(ref, oid) =>
                val (s1, ks) = s.rmObserver(ref, oid)
                (ks, s1, ())
              case RmAutoObserver(ref, cycle, oid) =>
                val (s1, ks) = s.rmObserver(ref, cycle, oid)
                (ks, s1, ())

              case SelTrigger(sel, f) =>
                val (s1, ok) = s.addSelTrigger(sel, f)
                (Lst.maybe(ok), s1, ())

              case ExclUpdate(ref, cycle, u, dom) =>
                (Lst.empty, s.exclUpdate[dom.Domain, dom.Update, dom.IDelta](ref, cycle, u)(dom), ())
              case Supply(ref, cycle, value) =>
                val (s1, ks) = s.supply(ref)(cycle, value)
                (ks, s1, ())
              case NewAutoCell(setup, dom) =>
                val (s1, ref) = s.newAutoCell(setup)(dom) // linter:ignore UndesirableTypeInference
                (Lst.empty, s1, ref)
              case AddFinalizer(ref, cycle, sub) =>
                s.addFinalizer(ref, cycle, sub)
              case RemoveFinalizer(ref, cycle, fid) =>
                val s1 = s.removeFinalizer(ref, cycle, fid)
                (Lst.empty, s1, ())
            }
          )
        }

      def uncons: Uncons[PropagationStore] = Uncons[PropagationStore](
        new `Forall{(* -> *) -> *}`[λ[K[_] => StateT[Option, PropagationStore[K], Lst[K[Unit]]]]] {
          override def compute[K[_]]: StateT[Option, PropagationStore[K], Lst[K[Unit]]] =
            StateT(_.uncons)
        })
    }

  override def fetchK[K[_], A](ref: ValK[K, A], s: StateK[K]): Option[A] =
    s.tryFetch(ref)

  override def fetchK[K[_], A](ref: VarK[K, A], s: StateK[K]): A =
    s.fetch(ref)

  override def isConsistent[K[_]](s: PropagationStore[K]): Boolean =
    s.failedVars.isEmpty

  override def stashable: StashOnDemandPropagationModule.AuxL[self.VarK, self.ValK, self.Lang] =
    new OnDemandPropagationListModule[self.VarK, self.ValK, self.Lang, self.StateK](this)
}


private[nutcracker] case class PropagationStore[K[_]] private(
  lastCellId: CellIdCounter,
  lastCellCycle: CellCycle[_],
  simpleCells: KMap[SimpleCellId[K, ?], SimpleCell[K, ?]],
  autoCells: KMap[AutoCellId[K, ?], OnDemandCell[K, ?]],
  selTriggers: KMapB[λ[`L <: HList` => Sel[CellId[K, ?], L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList],
  cellsToSels: Index[CellId[K, _], Sel[CellId[K, ?], _ <: HList]],
  failedVars: Set[SimpleCellId[K, _]],
  dirtyDomains: Set[CellId[K, _]],
  dirtySelections: Set[Sel[CellId[K, ?], _ <: HList]]
) {
  import shapeless.PolyDefns.~>

  type CellIncarnationId[A] = Cell.IncarnationId[K, A]
  type Tok[D, D0] = (CellIncarnationId[D], Token[D0])
  type PreHandler[D, Δ[_, _]] = SeqPreHandler[Tok[D, ?], K, D, Δ]
  type Trigger[D, Δ[_, _], D0] = SeqTrigger[Tok[D, ?], K, D, Δ, D0]

  // helpers for testing
  def fire[D](k: K[Unit])(implicit dom: IDom[D]): Trigger[D, dom.IDelta, D] = SeqTrigger.Fire[Tok[D, ?], K, D, dom.IDelta, D](k)
  def once[D](f: D => K[Unit])(implicit dom: IDom[D]): PreHandler[D, dom.IDelta] = SeqPreHandler[Tok[D, ?], K, D, dom.IDelta](d => fire[D](f(d)))

  private val cellFetcher: CellId[K, ?] ~> shapeless.Id = new (CellId[K, ?] ~> shapeless.Id) {
    def apply[D](cell: CellId[K, D]): D = fetch(cell)
  }

  def newCell[D](d: D)(implicit dom: IDom[D]): (PropagationStore[K], SimpleCellId[K, D]) = {
    val ref = lastCellId.inc[K, D]
    val simpleCells1 = simpleCells.put(ref)(SimpleCell.init(d))
    val failedVars1 = if(dom.isFailed(d)) failedVars + ref else failedVars
    (copy(lastCellId = ref.counter, simpleCells = simpleCells1, failedVars = failedVars1), ref)
  }

  def newAutoCell[D](setup: (AutoCellId[K, D], CellCycle[D]) => K[Unit])(implicit dom: IDom[D]): (PropagationStore[K], AutoCellId[K, D]) = {
    val ref = lastCellId.inc[K, D](setup)
    (copy(lastCellId = ref.counter), ref)
  }

  def tryFetch[D](ref: CellId[K, D]): Option[D] = ref match {
    case r @ SimpleCellId(_) => Some(simpleCells(r).value)
    case r @ AutoCellId(_, _) => autoCells.get(r).flatMap(_.getValue)
  }

  // unsafe, should only be allowed on simple cells
  def fetch[D](ref: CellId[K, D]): D = tryFetch(ref).get

  def fetchVector[D, N <: Nat](refs: Sized[Vector[SimpleCellId[K, D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def update[D, U, Δ[_, _]](ref: SimpleCellId[K, D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] =
    simpleCells(ref).infer.update(u) match {
      case None => this
      case Some(cell) =>
        val failedVars1 = if(dom.isFailed(cell.value)) failedVars + ref else failedVars
        val domains1 = simpleCells.put(ref)(cell)
        val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
        copy(simpleCells = domains1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }

  def exclUpdate[D, U, Δ[_, _]](ref: AutoCellId[K, D], cycle: CellCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        cell0.infer.exclUpdate(u) match {
          case None => this
          case Some(cell) =>
            val autoCells1 = autoCells.put(ref)(cell)
            val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
            copy(autoCells = autoCells1, dirtyDomains = dirtyDomains1)
        }
      case _ => this
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

  def resume[D, Δ[_, _], D0 <: D](ref: SimpleCellId[K, D], token: Token[D0], trigger: Trigger[D, Δ, D0]): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = simpleCells(ref).asInstanceOf[SimpleCell.AuxD[K, D, Δ]].resume(-\/(ref), token, trigger)
    val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
    (copy(simpleCells = simpleCells.put(ref)(cell), dirtyDomains = dirtyDomains1), ks)
  }

  def resume[D, Δ[_, _], D0 <: D](ref: AutoCellId[K, D], cycle: CellCycle[D], token: Token[D0], trigger: Trigger[D, Δ, D0]): (PropagationStore[K], Lst[K[Unit]]) =
    autoCells.get(ref) match {
      case Some(cell0) if(cell0.cycle === cycle) =>
        val (cell, ks) = cell0.asInstanceOf[OnDemandCell.AuxD[K, D, Δ]].resume(\/-((ref, cycle)), token, trigger)
        cell match {
          case Some(cell) =>
            val dirtyDomains1 = if (cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
            (copy(autoCells = autoCells.put(ref)(cell), dirtyDomains = dirtyDomains1), ks)
          case None =>
            (copy(autoCells = autoCells - ref), ks)
        }
      case _ =>
        (this, Lst.empty)
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

  def addSelTrigger[L <: HList](sel: Sel[CellId[K, ?], L], t: L => (Option[K[Unit]], Boolean)): (PropagationStore[K], Option[K[Unit]]) = {
    val (ko, keep) = t(sel.fetch(cellFetcher))
    if(keep) (addSelTrigger0(sel, t), ko)
    else     (this,                   ko)
  }


  private def addSelTrigger0[L <: HList](sel: Sel[CellId[K, ?], L], t: L => (Option[K[Unit]], Boolean)): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers.put(sel)(t :: selTriggers.getOrElse(sel)(Nil)),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  private def triggersForSel[L <: HList](sel: Sel[CellId[K, ?], L]): (PropagationStore[K], Lst[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectSelTriggers(d, selTriggers.getOrElse(sel)(Nil)) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers.put(sel)(forLater)), fired)
    }
  }

  private def getSelsForCell(ref: CellId[K, _]): Set[Sel[CellId[K, ?], _ <: HList]] = cellsToSels.get(ref)

  private def collectSelTriggers[L <: HList](l: L, triggers: List[L => (Option[K[Unit]], Boolean)]): (List[L => (Option[K[Unit]], Boolean)], Lst[K[Unit]]) =
    triggers match {
      case Nil => (Nil, Lst.empty)
      case t :: ts =>
        val (ts1, ks) = collectSelTriggers(l, ts)
        val (ko, keep) = t(l)
        if(keep) (t :: ts1, ko ?+: ks)
        else     (     ts1, ko ?+: ks)
    }

  def uncons: Option[(PropagationStore[K], Lst[K[Unit]])] =
    if(dirtyDomains.nonEmpty) {
      val ref0 = dirtyDomains.head
      val ref: CellId[K, ref0.Domain] = ref0.aux
      val dirtySels = dirtySelections union getSelsForCell(ref)
      ref match {
        case r @ SimpleCellId(_) =>
          val (cell, ks) = simpleCells(r).triggerPendingObservers(-\/(r))
          Some((copy(simpleCells = simpleCells.put(r)(cell), dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
        case r @ AutoCellId(_, _) =>
          val cell0 = autoCells(r)
          val (cell, ks) = cell0.triggerPendingObservers(\/-((r, cell0.cycle)))
          cell match {
            case Some(cell) =>
              Some((copy(autoCells = autoCells.put(r)(cell), dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
            case None =>
              Some((copy(autoCells = autoCells - r, dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
          }
      }
    } else if(dirtySelections.nonEmpty) {
      val sel = dirtySelections.head
      val (s1, ks) = triggersForSel(sel)
      Some((s1.copy(dirtySelections = dirtySelections.tail), ks))
    }
    else None
}

object PropagationStore {
  def empty[K[_]]: PropagationStore[K] = PropagationStore[K](
    lastCellId = CellIdCounter.zero,
    lastCellCycle = CellCycle.zero[Nothing],
    simpleCells = KMap[SimpleCellId[K, ?], SimpleCell[K, ?]](),
    autoCells = KMap[AutoCellId[K, ?], OnDemandCell[K, ?]](),
    selTriggers = KMapB[λ[`L <: HList` => Sel[CellId[K, ?], L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList](),
    cellsToSels = Index.empty(sel => sel.cells),
    failedVars = Set(),
    dirtyDomains = Set(),
    dirtySelections = Set()
  )
}

private[nutcracker] sealed abstract class Cell[K[_], D] {
  type Update
  type Delta[_, _]
  type Value <: D

  type CellIncarnationId = SimpleCellId[K, D] \/ (AutoCellId[K, D], CellCycle[D])
  type Tok[D1] = (CellIncarnationId, Token[D1])
  type PreHandler = SeqPreHandler[Tok, K, D, Delta]
  type Handler[D1] = SeqHandler[Tok, K, D, Delta, D1]
  type Trigger[D1] = SeqTrigger[Tok, K, D, Delta, D1]

  type IdleObserver[Val] = Cell.IdleObserver[K, D, Delta, Val]
  type PendingObserver[Val] = Cell.PendingObserver[K, D, Delta, Val]
  type BlockedIdleObserver[D0, Val] = Cell.BlockedIdleObserver[D, Delta, D0, Val]
  type BlockedPendingObserver[D0, Val] = Cell.BlockedPendingObserver[D, Delta, D0, Val]

  def hasPendingObservers: Boolean
}

private[nutcracker] object Cell {
  type AuxD[K[_], D, Δ[_, _]] = Cell[K, D] { type Delta[D1, D2] = Δ[D1, D2] }
  type Aux[K[_], D, U, Δ[_, _]] = Cell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  type IncarnationId[K[_], D] = SimpleCellId[K, D] \/ (AutoCellId[K, D], CellCycle[D])
  type Tok[K[_], D, D0] = (IncarnationId[K, D], Token[D0])

  final case class IdleObserver[K[_], D, Δ[_, _], Val](id: ObserverId, handler: SeqHandler[Tok[K, D, ?], K, D, Δ, Val]) {
    def addDelta[Val1](δ: Δ[Val, Val1]): PendingObserver.Aux[K, D, Δ, Val, Val1] =
      PendingObserver[K, D, Δ, Val, Val1](id, δ, handler)
  }
  sealed abstract class PendingObserver[K[_], D, Δ[_, _], Val](val id: ObserverId) {
    type D0
    val delta: Δ[D0, Val]
    val handler: SeqHandler[Tok[K, D, ?], K, D, Δ, D0]

    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): PendingObserver.Aux[K, D, Δ, D0, Val1] =
      PendingObserver[K, D, Δ, D0, Val1](id, dom.composeDeltas(δ, delta), handler)
  }
  object PendingObserver {
    type Aux[K[_], D, Δ[_, _], From, Val] = PendingObserver[K, D, Δ, Val] { type D0 = From }

    def apply[K[_], D, Δ[_, _], From, Val](id: ObserverId, delta: Δ[From, Val], handler: SeqHandler[Tok[K, D, ?], K, D, Δ, From]): Aux[K, D, Δ, From, Val] =
      new PendingObserver0(id, delta, handler)

    private final class PendingObserver0[K[_], D, Δ[_, _], From, Val](id: ObserverId, val delta: Δ[From, Val], val handler: SeqHandler[Tok[K, D, ?], K, D, Δ, From]) extends PendingObserver[K, D, Δ, Val](id) {
      type D0 = From
    }
  }
  final case class BlockedIdleObserver[D, Δ[_, _], Val, D0](id: ObserverId, ev: Val === D0) {
    def addDelta[Val1](δ: Δ[Val, Val1]): BlockedPendingObserver[D, Δ, Val1, D0] =
      BlockedPendingObserver(id, ev.subst[Δ[?, Val1]](δ))
    def resume[K[_]](handler: SeqHandler[Tok[K, D, ?], K, D, Δ, D0]): IdleObserver[K, D, Δ, Val] =
      IdleObserver(id, ev.flip.subst[SeqHandler[Tok[K, D, ?], K, D, Δ, ?]](handler))
  }
  object BlockedIdleObserver {
    def apply[D, Δ[_, _], D0](id: ObserverId): BlockedIdleObserver[D, Δ, D0, D0] = BlockedIdleObserver(id, Leibniz.refl[D0])
  }
  final case class BlockedPendingObserver[D, Δ[_, _], Val, D0](id: ObserverId, delta: Δ[D0, Val]) {
    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): BlockedPendingObserver[D, Δ, Val1, D0] =
      BlockedPendingObserver(id, dom.composeDeltas(δ, delta))
    def resume[K[_]](handler: SeqHandler[Tok[K, D, ?], K, D, Δ, D0]): PendingObserver.Aux[K, D, Δ, D0, Val] =
      PendingObserver(id, delta, handler)
  }

  def listRemoveFirst[A](l: List[A])(p: A => Boolean): Option[List[A]] = {
    @tailrec def go(revChecked: List[A], l: List[A]): Option[List[A]] = l match {
      case a :: as =>
        if(p(a)) Some(revChecked reverse_::: as)
        else go(a :: revChecked, as)
      case Nil =>
        None
    }

    go(Nil, l)
  }

  def mapRemoveFirst[Key[_], V[_]](m: KMap[Key, V])(p: ∃[V] => Boolean): Option[KMap[Key, V]] =
    m.find(p).map(m - _._1)
}

private[nutcracker] abstract class SimpleCell[K[_], D] extends Cell[K, D] {
  import nutcracker.Cell._

  val value: Value

  val idleObservers: List[IdleObserver[Value]]
  val pendingObservers: List[PendingObserver[Value]]
  val blockedIdleObservers: KMap[Token, BlockedIdleObserver[Value, ?]]
  val blockedPendingObservers: KMap[Token, BlockedPendingObserver[Value, ?]]

  val lastObserverId: ObserverId
  val lastToken: Token[_]


  def infer(implicit dom: IDom[D]): SimpleCell.Aux[K, D, dom.Update, dom.IDelta] =
    this.asInstanceOf[SimpleCell.Aux[K, D, dom.Update, dom.IDelta]]

  def aux1: SimpleCell.Aux1[K, D, Update, Delta, Value] = this

  def copy(
    idleObservers: List[IdleObserver[Value]] = idleObservers,
    pendingObservers: List[PendingObserver[Value]] = pendingObservers,
    blockedIdleObservers: KMap[Token, BlockedIdleObserver[Value, ?]] = blockedIdleObservers,
    blockedPendingObservers: KMap[Token, BlockedPendingObserver[Value, ?]] = blockedPendingObservers,
    nextObserverId: ObserverId = lastObserverId,
    lastToken: Token[_] = lastToken
  ): SimpleCell.Aux1[K, D, Update, Delta, Value] =
    SimpleCell[K, D, Update, Delta, Value](value)(idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, nextObserverId, lastToken)

  def hasObserver: Boolean = idleObservers.nonEmpty || pendingObservers.nonEmpty || blockedIdleObservers.nonEmpty || blockedPendingObservers.nonEmpty

  def hasPendingObservers: Boolean = pendingObservers.nonEmpty

  def update(u: Update)(implicit dom: IDom.Aux[D, Update, Delta]): Option[SimpleCell[K, D]] =
    dom.update(value, u) match {
      case up @ Updated(newVal, delta) =>
        val pending0 = pendingObservers.map(_.addDelta(delta))
        val pending1 = idleObservers.map(_.addDelta(delta))
        val pending = pending1 ::: pending0

        val blocked0 = blockedPendingObservers.mapValues[BlockedPendingObserver[up.NewValue, ?]](
          λ[BlockedPendingObserver[Value, ?] ~> BlockedPendingObserver[up.NewValue, ?]](_.addDelta(delta))
        )
        val blocked1 = blockedIdleObservers.mapValues[BlockedPendingObserver[up.NewValue, ?]](
          λ[BlockedIdleObserver[Value, ?] ~> BlockedPendingObserver[up.NewValue, ?]](_.addDelta(delta))
        )
        val blocked = blocked0 ++ blocked1

        Some(SimpleCell(newVal)(Nil, pending, KMap[Token, BlockedIdleObserver[up.NewValue, ?]](), blocked, lastObserverId, lastToken))

      case Unchanged() => None
    }

  def observe(self: CellIncarnationId, f: PreHandler): (SimpleCell.Aux1[K, D, Update, Delta, Value], Option[ObserverId], Lst[K[Unit]]) = {
    import SeqTrigger._
    f.handle(value) match {
      case Discard() => (this.aux1, None, Lst.empty)
      case Fire(k)   => (this.aux1, None, Lst.singleton(k))
      case Sleep(h)  =>
        val (cell, oid) = addObserver(h)
        (cell, Some(oid), Lst.empty)
      case FireReload(k, h) =>
        val (cell, oid) = addObserver(h)
        (cell, Some(oid), Lst.singleton(k))
      case Reconsider(cont) =>
        val (cell, token, oid) = block0
        (cell, Some(oid), Lst.singleton(cont((self, token))))
    }
  }

  def hold(f: (D, Token[D], ObserverId) => K[Unit]): (SimpleCell.Aux1[K, D, Update, Delta, Value], ObserverId, Lst[K[Unit]]) = {
    val (cell, token, oid) = block0
    val k = f(value, token, oid)
    (cell, oid, Lst.singleton(k))
  }

  private def block0: (SimpleCell.Aux1[K, D, Update, Delta, Value], Token[Value], ObserverId) = {
    val token = lastToken.inc[Value]
    val oid = lastObserverId.inc
    (SimpleCell[K, D, Update, Delta, Value](value)(idleObservers, pendingObservers, blockedIdleObservers.put(token)(BlockedIdleObserver(oid)), blockedPendingObservers, oid, token), token, oid)
  }

  private def addObserver(f: Handler[Value]): (SimpleCell.Aux1[K, D, Update, Delta, Value], ObserverId) = {
    val oid = lastObserverId.inc
    val obs = IdleObserver(oid, f)
    (SimpleCell[K, D, Update, Delta, Value](value)(obs :: idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, oid, lastToken), oid)
  }

  def rmObserver(oid: ObserverId): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) =
    (rmObserver0(oid), Lst.empty)

  def rmObserver0(oid: ObserverId): SimpleCell.Aux1[K, D, Update, Delta, Value] = {
    listRemoveFirst(idleObservers)(_.id === oid) match {
      case Some(idles) => copy(idleObservers = idles)
      case None => listRemoveFirst(pendingObservers)(_.id === oid) match {
        case Some(pendings) => copy(pendingObservers = pendings)
        case None => mapRemoveFirst(blockedIdleObservers)(_.id === oid) match {
          case Some(blockedIdles) => copy(blockedIdleObservers = blockedIdles)
          case None => mapRemoveFirst(blockedPendingObservers)(_.id === oid) match {
            case Some(blockedPendings) => copy(blockedPendingObservers = blockedPendings)
            case None => this
          }
        }
      }
    }
  }

  def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
    import SeqTrigger._
    trigger match {
      case Discard() => (copy(blockedIdleObservers = blockedIdleObservers - token, blockedPendingObservers = blockedPendingObservers - token), Lst.empty)
      case Fire(k)   => (copy(blockedIdleObservers = blockedIdleObservers - token, blockedPendingObservers = blockedPendingObservers - token), Lst.singleton(k))
      case Sleep(handler) =>
        (resumeWithHandler(token, handler), Lst.empty)
      case FireReload(k, handler) =>
        (resumeWithHandler(token, handler), Lst.singleton(k))
      case Reconsider(f) =>
        val nextToken = lastToken.inc[D0]
        val k = f((self, nextToken))
        val cell = blockedIdleObservers.get(token) match {
          case Some(obs) =>
            assert(blockedPendingObservers.get(token).isEmpty)
            copy(
              blockedIdleObservers = (blockedIdleObservers - token).put(nextToken)(obs),
              lastToken = nextToken
            )
          case None => blockedPendingObservers.get(token) match {
            case Some(obs) =>
              copy(
                blockedPendingObservers = (blockedPendingObservers - token).put(nextToken)(obs),
                lastToken = nextToken
              )
            case None =>
              sys.error(s"unrecognized token $token")
          }
        }
        (cell, Lst.singleton(k))
    }
  }

  private def resumeWithHandler[D0 <: D](token: Token[D0], handler: Handler[D0]): SimpleCell.Aux1[K, D, Update, Delta, Value] =
    blockedIdleObservers.get(token) match {
      case Some(obs) =>
        assert(blockedPendingObservers.get(token).isEmpty)
        val obs1 = obs.resume(handler)
        SimpleCell[K, D, Update, Delta, Value](value)(obs1 :: idleObservers, pendingObservers, blockedIdleObservers - token, blockedPendingObservers, lastObserverId, lastToken)
      case None => blockedPendingObservers.get(token) match {
        case Some(obs) =>
          val obs1 = obs.resume(handler)
          SimpleCell[K, D, Update, Delta, Value](value)(idleObservers, obs1 :: pendingObservers, blockedIdleObservers, blockedPendingObservers - token, lastObserverId, lastToken)
        case None =>
          sys.error(s"unrecognized token $token")
      }
    }

  def triggerPendingObservers(self: CellIncarnationId): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
    // @tailrec // can't use because pattern matching sucks, so we use Church encoding (fold)
    def go(
      pending: List[PendingObserver[Value]],
      idleAcc: List[IdleObserver[Value]],
      blockedIdleAcc: KMap[Token, BlockedIdleObserver[Value, ?]],
      firedAcc: Lst[K[Unit]],
      lastToken: Token[_]
    ): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) =
    pending match {
      case Nil => (SimpleCell[K, D, Update, Delta, Value](value)(idleAcc, Nil, blockedIdleAcc, blockedPendingObservers, lastObserverId, lastToken), firedAcc)
      case po :: tail =>
        po.handler.handle(value, po.delta).fold(
          caseDiscard = go(tail, idleAcc, blockedIdleAcc, firedAcc, lastToken),
          caseFire = k => go(tail, idleAcc, blockedIdleAcc, k :: firedAcc, lastToken),
          caseSleep = h => go(tail, IdleObserver(po.id, h) :: idleAcc, blockedIdleAcc, firedAcc, lastToken),
          caseFireReload = (k, h) => go(tail, IdleObserver(po.id, h) :: idleAcc, blockedIdleAcc, k :: firedAcc, lastToken),
          caseReconsider = f => {
            val token = lastToken.inc[Value]
            val k = f((self, token))
            go(tail, idleAcc, blockedIdleAcc.put(token)(BlockedIdleObserver(po.id, Leibniz.refl[Value])), k :: firedAcc, token)
          }
        )
    }

    go(pendingObservers, idleObservers, blockedIdleObservers, Lst.empty, lastToken)
  }
}

private[nutcracker] object SimpleCell {
  import Cell._

  type AuxD[K[_], D, Δ[_, _]] = SimpleCell[K, D] { type Delta[D1, D2] = Δ[D1, D2] }
  type Aux[K[_], D, U, Δ[_, _]] = SimpleCell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }
  type Aux1[K[_], D, U, Δ[_, _], Val] = SimpleCell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2]; type Value = Val }

  def init[K[_], D](d: D)(implicit dom: IDom[D]): SimpleCell.Aux[K, D, dom.Update, dom.IDelta] =
    SimpleCell[K, D, dom.Update, dom.IDelta, D](d)(
      Nil,
      Nil,
      KMap(),
      KMap(),
      ObserverId.zero,
      Token.zero
    )

  def apply[K[_], D, U, Δ[_, _], Val <: D](d: Val)(
    idleObservers0: List[IdleObserver[K, D, Δ, Val]],
    pendingObservers0: List[PendingObserver[K, D, Δ, Val]] = Nil,
    blockedIdleObservers0: KMap[Token, BlockedIdleObserver[D, Δ, Val, ?]],
    blockedPendingObservers0: KMap[Token, BlockedPendingObserver[D, Δ, Val, ?]] = KMap[Token, BlockedPendingObserver[D, Δ, Val, ?]](), // https://github.com/scala/scala-dev/issues/366
    lastObsId: ObserverId,
    lastTok: Token[_]
  ): SimpleCell.Aux1[K, D, U, Δ, Val] = new SimpleCell[K, D] {
    type Update = U
    type Delta[D1, D2] = Δ[D1, D2]
    type Value = Val

    val value = d
    val idleObservers = idleObservers0
    val pendingObservers = pendingObservers0
    val blockedIdleObservers = blockedIdleObservers0
    val blockedPendingObservers = blockedPendingObservers0
    val lastObserverId = lastObsId
    val lastToken = lastTok
  }
}

private[nutcracker] sealed abstract class OnDemandCell[K[_], D] extends Cell[K, D] {
  val cycle: CellCycle[D]

  def getValue: Option[Value]

  def observe(self: CellIncarnationId, f: SeqPreHandler[Tok, K, D, Delta]): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]])

  def rmObserver(oid: ObserverId): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def hold(f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]])

  def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def supply[D0 <: D](self: CellIncarnationId, value: D0): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def exclUpdate(u: Update)(implicit dom: IDom.Aux[D, Update, Delta]): Option[OnDemandCell[K, D]]

  def addFinalizer(sub: Subscription[K]): (Lst[K[Unit]], OnDemandCell[K, D], FinalizerId)

  def removeFinalizer(fid: FinalizerId): OnDemandCell[K, D]

  def triggerPendingObservers(self: CellIncarnationId): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def infer(implicit dom: IDom[D]): OnDemandCell.Aux[K, D, dom.Update, dom.IDelta] =
    this.asInstanceOf[OnDemandCell.Aux[K, D, dom.Update, dom.IDelta]]
}

object OnDemandCell {
  type AuxD[K[_], D, Δ[_, _]] = OnDemandCell[K, D] { type Delta[D0, D1] = Δ[D0, D1] }
  type Aux[K[_], D, U, Δ[_, _]] = OnDemandCell[K, D] { type Update = U; type Delta[D0, D1] = Δ[D0, D1] }
}

private[nutcracker] case class InitializingCell[K[_], D, U, Δ[_, _]](
  cycle: CellCycle[D],
  preHandlers: List[(ObserverId, SeqPreHandler[λ[α => (Cell.IncarnationId[K, D], Token[α])], K, D, Δ])],
  preHandlersM: List[(ObserverId, (D, CellCycle[D], Token[D], ObserverId) => K[Unit])],
  lastObserverId: ObserverId
) extends OnDemandCell[K, D] {
  import Cell._

  type Update = U
  type Delta[D0, D1] = Δ[D0, D1]
  type Value = Nothing

  require(preHandlers.nonEmpty || preHandlersM.nonEmpty)

  private val observerCount = preHandlers.size + preHandlersM.size

  override def getValue: Option[Value] = None

  override def hasPendingObservers: Boolean = false

  override def observe(self: CellIncarnationId, f: PreHandler): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = copy[K, D, U, Δ](preHandlers = (obsId, f) :: preHandlers, lastObserverId = obsId)
    (cell, Some(obsId), Lst.empty)
  }

  override def hold(f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = copy[K, D, U, Δ](preHandlersM = (obsId, f) :: preHandlersM, lastObserverId = obsId)
    (cell, obsId, Lst.empty)
  }

  override def supply[D0 <: D](self: CellIncarnationId, value: D0): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    var idleObservers: List[IdleObserver[D0]] = Nil
    var blockedIdleObservers: KMap[Token, BlockedIdleObserver[D0, ?]] = KMap()
    var ks: Lst[K[Unit]] = Lst.empty
    var token: Token[_] = Token.zero

    preHandlers.foreach({ case (id, h) => h.handle(value).fold(
      caseDiscard = (), // do nothing
      caseFire = k => { ks = k :: ks },
      caseSleep = handler => { idleObservers = IdleObserver(id, handler) :: idleObservers },
      caseFireReload = (k, handler) => {
        ks = k :: ks
        idleObservers = IdleObserver(id, handler) :: idleObservers
      },
      caseReconsider = cont => {
        val tok = token.inc[D0]
        ks = cont((self, tok)) :: ks
        blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver(id))
        token = tok
      }
    )})

    preHandlersM.foreach({ case (id, f) => {
      val tok = token.inc[D0]
      ks = f(value, cycle, tok, id) :: ks
      blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver(id))
      token = tok
    }})

    if(idleObservers.nonEmpty || blockedIdleObservers.nonEmpty) {
      val cell = new ActiveCell(cycle, Map(), FinalizerId.zero, lastObserverId, token, value, idleObservers, blockedIdleObservers)
      (Some(cell), ks)
    } else {
      (None, ks)
    }
  }

  override def exclUpdate(u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[OnDemandCell[K, D]] = {
    sys.error("Unreachable code: no one has access to the current cycle yet")
  }

  override def addFinalizer(sub: Subscription[K]): (Lst[K[Unit]], OnDemandCell[K, D], FinalizerId) =
    sys.error("Unreachable code: no one has access to the current cycle yet")

  override def removeFinalizer(fid: FinalizerId): OnDemandCell[K, D] =
    sys.error("Unreachable code: no one has access to the current cycle yet")

  override def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (Option[OnDemandCell.Aux[K, D, U, Δ]], Lst[K[Unit]]) =
    sys.error("No tokens issued in this cycle yet")

  override def triggerPendingObservers(self: CellIncarnationId): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) =
    // there are no pending observers
    (Some(this), Lst.empty)

  override def rmObserver(oid: ObserverId): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    val cell = listRemoveFirst(preHandlers)(_._1 === oid) match {
      case Some(preHandlers) =>
        if(observerCount == 1) None
        else Some(copy(preHandlers = preHandlers))
      case None => listRemoveFirst(preHandlersM)(_._1 === oid) match {
        case Some(preHandlersM) =>
          if(observerCount == 1) None
          else Some(copy(preHandlersM = preHandlersM))
        case None => sys.error("Non-existent observer (probably trying to remove an observer twice)")
      }
    }
    (cell, Lst.empty)
  }
}

object InitializingCell {
  def init[K[_], D, U, Δ[_, _]](cycle: CellCycle[D], f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): (InitializingCell[K, D, U, Δ], ObserverId) = {
    val obsId = ObserverId.zero
    (InitializingCell(cycle, Nil, (obsId, f) :: Nil, obsId), obsId)
  }

  def init[K[_], D, U, Δ[_, _]](cycle: CellCycle[D], f: SeqPreHandler[λ[α => (Cell.IncarnationId[K, D], Token[α])], K, D, Δ]): (InitializingCell[K, D, U, Δ], ObserverId) = {
    val obsId = ObserverId.zero
    (InitializingCell(cycle, (obsId, f) :: Nil, Nil, obsId), obsId)
  }
}

private[nutcracker] case class ActiveCell[K[_], D, U, Δ[_, _], Val <: D](
  cycle: CellCycle[D],
  impl: SimpleCell.Aux1[K, D, U, Δ, Val],
  finalizers: Map[FinalizerId, Subscription[K]],
  lastFinalizerId: FinalizerId
) extends OnDemandCell[K, D] {
  type Update = U
  type Delta[D0, D1] = Δ[D0, D1]
  type Value = Val

  require(impl.hasObserver)

  def this(
    cycle: CellCycle[D],
    finalizers: Map[FinalizerId, Subscription[K]],
    lastFinalizerId: FinalizerId,
    lastObserverId: ObserverId,
    lastToken: Token[_],
    value: Val,
    idleObservers: List[Cell.IdleObserver[K, D, Δ, Val]],
    blockedIdleObservers: KMap[Token, Cell.BlockedIdleObserver[D, Δ, Val, ?]]
  ) = this(
    cycle,
    SimpleCell[K, D, U, Δ, Val](value)(idleObservers0 = idleObservers, blockedIdleObservers0 = blockedIdleObservers, lastObsId = lastObserverId, lastTok = lastToken),
    finalizers,
    lastFinalizerId
  )

  override def getValue: Option[Value] = Some(impl.value)

  override def hasPendingObservers: Boolean = impl.hasPendingObservers

  override def observe(self: CellIncarnationId, f: PreHandler): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]]) = {
    val (cell, oid, ks) = impl.observe(self, f)
    (copy(impl = cell), oid, ks)
  }

  override def hold(f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]]) = {
    val (cell, oid, ks) = impl.hold(f(_, cycle, _, _))
    (copy(impl = cell), oid, ks)
  }

  override def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    val (cell, ks) = impl.resume(self, token, trigger)
    if(cell.hasObserver) (Some(copy(impl = cell)), ks)
    else (None, ks ++ collectFinalizers)
  }

  override def rmObserver(oid: ObserverId): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    val cell = impl.rmObserver0(oid)
    if(cell.hasObserver) (Some(copy(impl = cell)), Lst.empty)
    else (None, collectFinalizers)
  }

  override def supply[D0 <: D](self: CellIncarnationId, value: D0): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) =
    sys.error("trying to initialize a cell twice in the same cell cycle")

  override def triggerPendingObservers(self: CellIncarnationId): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    val (cell, ks) = impl.triggerPendingObservers(self)
    if(cell.hasObserver) (Some(copy(impl = cell)), ks)
    else (None, ks ++ collectFinalizers)
  }

  override def exclUpdate(u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[OnDemandCell[K, D]] = {
    impl.update(u) match { // linter:ignore UseOptionMapNotPatMatch
      case Some(cell) => Some(ActiveCell[K, D, cell.Update, cell.Delta, cell.Value](cycle, cell, finalizers, lastFinalizerId))
      case None => None
    }
  }

  override def addFinalizer(sub: Subscription[K]): (Lst[K[Unit]], OnDemandCell[K, D], FinalizerId) = {
    val fid = lastFinalizerId.inc
    (Lst.empty, copy(finalizers = finalizers.updated(fid, sub), lastFinalizerId = fid), fid)
  }

  override def removeFinalizer(fid: FinalizerId): OnDemandCell[K, D] =
    copy(finalizers = finalizers - fid)

  private def collectFinalizers: Lst[K[Unit]] =
    finalizers.valuesIterator.foldLeft(Lst.empty[K[Unit]])((ks, sub) => sub.unsubscribe ++ ks)
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

  implicit def orderKInstance[K[_]]: HOrderK[CellId[K, ?]] = new HOrderK[CellId[K, ?]] {
    override def hOrderK[A, B](fa: CellId[K, A], fb: CellId[K, B]): Ordering =
      CellId.order(fa, fb)
  }

  implicit def showKInstance[K[_]]: ShowK[CellId[K, ?]] = new ShowK[CellId[K, ?]] {
    def shows[A](ref: CellId[K, A]): String = s"cell${ref.domainId}"
  }
}

private[nutcracker] final case class SimpleCellId[K[_], D](domainId: Long) extends CellId[K, D] {
  override def aux: SimpleCellId.Aux[K, Domain, Update, Delta] = this
}

private[nutcracker] object SimpleCellId {
  type Aux[K[_], D, U, Δ[_, _]] = SimpleCellId[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  implicit def orderKInstance[K[_]]: HOrderK[SimpleCellId[K, ?]] = new HOrderK[SimpleCellId[K, ?]] {
    override def hOrderK[A, B](fa: SimpleCellId[K, A], fb: SimpleCellId[K, B]): Ordering =
      CellId.order(fa, fb)
  }

  implicit def showInstance[K[_], D]: Show[SimpleCellId[K, D]] = new Show[SimpleCellId[K, D]] {
    override def shows(ref: SimpleCellId[K, D]): String = s"ref${ref.domainId}"
  }

  implicit def showKInstance[K[_]]: ShowK[SimpleCellId[K, ?]] = new ShowK[SimpleCellId[K, ?]] {
    def shows[A](ref: SimpleCellId[K, A]): String = s"cell${ref.domainId}"
  }
}

private[nutcracker] final case class AutoCellId[K[_], A](domainId: Long, runSetup: (AutoCellId[K, A], CellCycle[A]) => K[Unit]) extends CellId[K, A] {
  def setup(cycle: CellCycle[A]): K[Unit] = runSetup(this, cycle)
}

private[nutcracker] final class Token[+A] private(val id: Long) extends AnyVal {
  def inc[B]: Token[B] = new Token(id + 1)
}
private[nutcracker] object Token {
  def zero[A]: Token[A] = new Token(0)
}

private[nutcracker] final class ObserverId private(val id: Long) extends AnyVal {
  def inc: ObserverId = new ObserverId(id + 1)
}
private[nutcracker] object ObserverId {
  def zero: ObserverId = new ObserverId(0)

  implicit val equalInstance: Equal[ObserverId] = new Equal[ObserverId] {
    def equal(a1: ObserverId, a2: ObserverId): Boolean = a1.id == a2.id
  }
}

private[nutcracker] final class FinalizerId private(val id: Long) extends AnyVal {
  def inc: FinalizerId = new FinalizerId(id + 1)
}
private[nutcracker] object FinalizerId {
  def zero: FinalizerId = new FinalizerId(0)

  implicit val equalInstance: Equal[FinalizerId] = new Equal[FinalizerId] {
    def equal(a1: FinalizerId, a2: FinalizerId): Boolean = a1.id == a2.id
  }
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