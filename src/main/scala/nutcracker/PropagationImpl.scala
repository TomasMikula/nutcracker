package nutcracker

import nutcracker.CellCycle.{LiveCycle, SporeCycle}
import nutcracker.util.{FreeK, FreeKT, HEqualK, HOrderK, Index, InjectK, KMap, KMapB, Lst, ShowK, StateInterpreter, Step, Uncons, WriterState, `Forall{(* -> *) -> *}`, ∃}
import scala.annotation.tailrec
import scalaz.{Equal, Functor, IndexedContT, Leibniz, Monad, Ordering, Show, StateT, ~>}
import scalaz.Id.Id
import scalaz.std.option._
import scalaz.Leibniz.===
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import shapeless.{HList, Nat, Sized}

private[nutcracker] object PropagationImpl extends PersistentOnDemandPropagationModule with PropagationBundle { self =>
  type Var[A] = CellId[A]
  type Val[A] = CellId[A]
  type Lang[K[_], A] = PropagationLang[Var, K, A]
  type State[K[_]] = PropagationStore[K]

  implicit val varEquality: HEqualK[Var] = CellId.equalKInstance
  implicit def varOrder: HOrderK[Var] = CellId.orderKInstance
  implicit def varShow: ShowK[Var] = CellId.showKInstance
  implicit val valEquality: HEqualK[Var] = CellId.equalKInstance
  implicit def valOrder: HOrderK[Var] = CellId.orderKInstance
  implicit def valShow: ShowK[Var] = CellId.showKInstance

  implicit def prgMonad: Monad[FreeK[Lang, ?]] = FreeKT.freeKTMonad[Lang, Id]
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): OnDemandPropagation[FreeK[F, ?], Var, Val] =
    PropagationLang.freePropagation[F]

  val propagationApi: Propagation[Prg, Var, Val] =
    PropagationLang.freePropagation[Lang]

  def empty[K[_]]: PropagationStore[K] =
    PropagationStore[K](
      lastId = CellId.zero,
      domains = KMap[CellId, Cell[K, ?]](),
      selTriggers = KMapB[λ[`L <: HList` => Sel[CellId, L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList](),
      cellsToSels = Index.empty(sel => sel.cells),
      failedVars = Set(),
      dirtyDomains = Set(),
      dirtySelections = Set()
    )

  def interpret[A](p: Prg[A], s: PropagationStore[Prg]): (PropagationStore[Prg], A) =
    interpreter.freeInstance.apply(p).run(s)

  val interpreter: StateInterpreter[Lang, State] =
    new StateInterpreter[PropagationLang[Var, ?[_], ?], PropagationStore] {

      def step: Step[PropagationLang[Var, ?[_], ?], PropagationStore] =
        new Step[PropagationLang[Var, ?[_], ?], PropagationStore] {
          import PropagationLang._
          override def apply[K[_]: Monad, A](p: PropagationLang[Var, K, A]): WriterState[Lst[K[Unit]], PropagationStore[K], A] = WriterState(s =>
            p match {
              case Update(ref, u, dom) =>
                (Lst.empty, s.update[dom.Domain, dom.Update, dom.IDelta](ref, u)(dom), ())
              case Observe(ref, f, dom) =>
                val (s1, oo, ko) = s.addDomainObserver[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom)
                (Lst.maybe(ko), s1, oo)
              case NewCell(d, dom) =>
                val (s1, ref: CellId[dom.Domain]) = s.newCell(d)(dom)
                (Lst.empty, s1, ref)

              case Hold(ref, f) =>
                val (s1, oid, ko) = s.hold(ref)(f)
                (Lst.maybe(ko), s1, oid)
              case r @ Resume(ref, token, trigger) =>
                val (s1, ks) = s.resume[r.Domain, r.Delta, r.Arg](ref, token, trigger)
                (ks, s1, ())
              case RmObserver(ref, oid) =>
                val (s1, ks) = s.rmObserver(ref, oid)
                (ks, s1, ())

              case SelTrigger(sel, f) =>
                val (s1, ok) = s.addSelTrigger(sel, f)
                (Lst.maybe(ok), s1, ())

              case ExclUpdate(ref, cycle, u, dom) =>
                (Lst.empty, s.exclUpdate[dom.Domain, dom.Update, dom.IDelta](ref, cycle, u)(dom), ())
              case Supply(ref, cycle, value) =>
                val (s1, ks) = s.supply(ref)(cycle, value)
                (ks, s1, ())
              case NewAutoCell(setup, supply, dom, ftor) =>
                val (s1, ref: CellId[dom.Domain]) = s.newAutoCell(setup, supply)(dom, ftor)
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

  def fetch[K[_], D](ref: Var[D], s: State[K]): D =
    s.fetch(ref)

  def isConsistent[K[_]](s: PropagationStore[K]): Boolean =
    s.failedVars.isEmpty

  def stashable: StashOnDemandPropagationModule { type Var[A] = self.Var[A]; type Val[A] = self.Val[A]; type Lang[K[_], A] = self.Lang[K, A] } =
    new OnDemandPropagationListModule[self.Var, self.Val, self.Lang, self.State](this)
}


private[nutcracker] case class PropagationStore[K[_]] private(
  lastId: CellId[_],
  domains: KMap[CellId, Cell[K, ?]],
  selTriggers: KMapB[λ[`L <: HList` => Sel[CellId, L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList],
  cellsToSels: Index[CellId[_], Sel[CellId, _ <: HList]],
  failedVars: Set[CellId[_]],
  dirtyDomains: Set[CellId[_]],
  dirtySelections: Set[Sel[CellId, _ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: CellId ~> shapeless.Id = new (CellId ~> shapeless.Id) {
    def apply[D](cell: CellId[D]): D = fetch(cell)
  }

  def newCell[D](d: D)(implicit dom: IDom[D]): (PropagationStore[K], CellId[D]) = {
    val ref = lastId.inc[D]
    val domains1 = domains.put(ref)(SimpleCell.init(d))
    val failedVars1 = if(dom.isFailed(d)) failedVars + ref else failedVars
    (copy(lastId = ref, domains = domains1, failedVars = failedVars1), ref)
  }

  def newAutoCell[D](setup: IndexedContT[K, Unit, (CellId[D], LiveCycle[D]), D], supply: (CellId[D], LiveCycle[D], D) => K[Unit])(implicit dom: IDom[D], K: Functor[K]): (PropagationStore[K], CellId[D]) = {
    val ref = lastId.inc[D]
    val setup1 = (c: LiveCycle[D]) => setup.run(d => supply(ref, c, d).as((ref, c)))
    val cell = InactiveCell.init[K, D, dom.Update, dom.IDelta](setup1)
    (copy(lastId = ref, domains = domains.put(ref)(cell)), ref)
  }

  def tryFetch[D](ref: CellId[D]): Option[D] = domains(ref).getValue

  // unsafe, should only be allowed on simple cells
  def fetch[D](ref: CellId[D]): D = tryFetch(ref).get

  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellId[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def update[D, U, Δ[_, _]](ref: CellId[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] =
    domains(ref).infer.unsafeAsSimple.update(u) match {
      case None => this
      case Some(cell) =>
        val failedVars1 = if(cell.getValue.fold(false)(dom.isFailed(_))) failedVars + ref else failedVars
        val domains1 = domains.put(ref)(cell)
        val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
        copy(domains = domains1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }

  def exclUpdate[D, U, Δ[_, _]](ref: CellId[D], cycle: LiveCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] = {
    domains(ref).infer.unsafeAsAuto.exclUpdate(cycle, u) match {
      case None => this
      case Some(cell) =>
        val domains1 = domains.put(ref)(cell)
        val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
        copy(domains = domains1, dirtyDomains = dirtyDomains1)
    }
  }

  def addDomainObserver[D, U, Δ[_, _]](ref: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Option[ObserverId], Option[K[Unit]]) = {
    val (cell, oid, ko) = domains(ref).infer.observe(ref, f)
    (copy(domains = domains.put(ref)(cell)), oid, ko)
  }

  def hold[D](ref: CellId[D])(f: (D, Token[D], ObserverId) => K[Unit]): (PropagationStore[K], ObserverId, Option[K[Unit]]) = {
    val (cell, obsId, ko) = domains(ref).hold(f)
    (copy(domains = domains.put(ref)(cell)), obsId, ko)
  }

  def supply[D](ref: CellId[D])(cycle: LiveCycle[D], value: D): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = domains(ref).unsafeAsAuto.supply(ref, cycle, value)
    (copy(domains = domains.put(ref)(cell)), ks)
  }

  def resume[D, Δ[_, _], D0 <: D](ref: CellId[D], token: Token[D0], trigger: SeqTrigger[Token, K[Unit], D, Δ, D0]): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = domains(ref).asInstanceOf[Cell.AuxD[K, D, Δ]].resume(ref, token, trigger)
    val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
    (copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains1), ks)
  }

  def rmObserver[D](ref: CellId[D], oid: ObserverId): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = domains(ref).rmObserver(oid)
    (copy(domains = domains.put(ref)(cell)), ks)
  }

  def addFinalizer[A](ref: CellId[A], cycle: LiveCycle[A], sub: Subscription[K]): (Lst[K[Unit]], PropagationStore[K], Option[FinalizerId]) = {
    val (ks, cell, fid) = domains(ref).unsafeAsAuto.addFinalizer(cycle, sub)
    (ks, copy(domains = domains.put(ref)(cell)), fid)
  }

  def removeFinalizer[A](ref: CellId[A], cycle: LiveCycle[A], fid: FinalizerId): PropagationStore[K] = {
    val cell = domains(ref).unsafeAsAuto.removeFinalizer(cycle, fid)
    copy(domains = domains.put(ref)(cell))
  }

  def addSelTrigger[L <: HList](sel: Sel[CellId, L], t: L => (Option[K[Unit]], Boolean)): (PropagationStore[K], Option[K[Unit]]) = {
    val (ko, keep) = t(sel.fetch(cellFetcher))
    if(keep) (addSelTrigger0(sel, t), ko)
    else     (this,                   ko)
  }


  private def addSelTrigger0[L <: HList](sel: Sel[CellId, L], t: L => (Option[K[Unit]], Boolean)): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers.put(sel)(t :: selTriggers.getOrElse(sel)(Nil)),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  private def triggersForSel[L <: HList](sel: Sel[CellId, L]): (PropagationStore[K], Lst[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectSelTriggers(d, selTriggers.getOrElse(sel)(Nil)) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers.put(sel)(forLater)), fired)
    }
  }

  private def getSelsForCell(ref: CellId[_]): Set[Sel[CellId, _ <: HList]] = cellsToSels.get(ref)

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
      val ref: CellId.Aux[ref0.Domain, ref0.Update, ref0.Delta] = ref0.aux
      val dirtySels = dirtySelections union getSelsForCell(ref)
      val (cell, ks) = domains(ref).triggerPendingObservers(ref)
      Some((copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
    } else if(dirtySelections.nonEmpty) {
      val sel = dirtySelections.head
      val (s1, ks) = triggersForSel(sel)
      Some((s1.copy(dirtySelections = dirtySelections.tail), ks))
    }
    else None
}

private[nutcracker] sealed abstract class Cell[K[_], D] {
  type Update
  type Delta[_, _]
  type Value <: D

  type Handler[D1] = SeqHandler[Token, K[Unit], D, Delta, D1]
  type Trigger[D1] = SeqTrigger[Token, K[Unit], D, Delta, D1]

  type IdleObserver[Val] = Cell.IdleObserver[K, D, Delta, Val]
  type PendingObserver[Val] = Cell.PendingObserver[K, D, Delta, Val]
  type BlockedIdleObserver[D0, Val] = Cell.BlockedIdleObserver[D, Delta, D0, Val]
  type BlockedPendingObserver[D0, Val] = Cell.BlockedPendingObserver[D, Delta, D0, Val]

  def infer(implicit dom: IDom[D]): Cell.Aux[K, D, dom.Update, dom.IDelta] =
    this.asInstanceOf[Cell.Aux[K, D, dom.Update, dom.IDelta]]

  def unsafeAsSimple: SimpleCell.Aux[K, D, Update, Delta] = this.asInstanceOf[SimpleCell.Aux[K, D, Update, Delta]]
  def unsafeAsAuto: OnDemandCell[K, D, Update, Delta] = this.asInstanceOf[OnDemandCell[K, D, Update, Delta]]

  def hasPendingObservers: Boolean

  def getValue: Option[Value]

  def observe(self: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Delta]): (Cell[K, D], Option[ObserverId], Option[K[Unit]])

  def rmObserver(oid: ObserverId): (Cell[K, D], Lst[K[Unit]])

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def hold(f: (D, Token[D], ObserverId) => K[Unit]): (Cell[K, D], ObserverId, Option[K[Unit]])

  def resume[D0 <: D](self: CellId[D], token: Token[D0], trigger: Trigger[D0]): (Cell[K, D], Lst[K[Unit]])

  def triggerPendingObservers(self: CellId[D]): (Cell[K, D], Lst[K[Unit]])
}

private[nutcracker] object Cell {
  type AuxD[K[_], D, Δ[_, _]] = Cell[K, D] { type Delta[D1, D2] = Δ[D1, D2] }
  type Aux[K[_], D, U, Δ[_, _]] = Cell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  final case class IdleObserver[K[_], D, Δ[_, _], Val](id: ObserverId, handler: SeqHandler[Token, K[Unit], D, Δ, Val]) {
    def addDelta[Val1](δ: Δ[Val, Val1]): PendingObserver.Aux[K, D, Δ, Val, Val1] =
      PendingObserver[K, D, Δ, Val, Val1](id, δ, handler)
  }
  sealed abstract class PendingObserver[K[_], D, Δ[_, _], Val](val id: ObserverId) {
    type D0
    val delta: Δ[D0, Val]
    val handler: SeqHandler[Token, K[Unit], D, Δ, D0]

    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): PendingObserver.Aux[K, D, Δ, D0, Val1] =
      PendingObserver[K, D, Δ, D0, Val1](id, dom.composeDeltas(δ, delta), handler)
  }
  object PendingObserver {
    type Aux[K[_], D, Δ[_, _], From, Val] = PendingObserver[K, D, Δ, Val] { type D0 = From }

    def apply[K[_], D, Δ[_, _], From, Val](id: ObserverId, delta: Δ[From, Val], handler: SeqHandler[Token, K[Unit], D, Δ, From]): Aux[K, D, Δ, From, Val] =
      new PendingObserver0(id, delta, handler)

    private final class PendingObserver0[K[_], D, Δ[_, _], From, Val](id: ObserverId, val delta: Δ[From, Val], val handler: SeqHandler[Token, K[Unit], D, Δ, From]) extends PendingObserver[K, D, Δ, Val](id) {
      type D0 = From
    }
  }
  final case class BlockedIdleObserver[D, Δ[_, _], Val, D0](id: ObserverId, ev: Val === D0) {
    def addDelta[Val1](δ: Δ[Val, Val1]): BlockedPendingObserver[D, Δ, Val1, D0] =
      BlockedPendingObserver(id, ev.subst[Δ[?, Val1]](δ))
    def resume[K[_]](handler: SeqHandler[Token, K[Unit], D, Δ, D0]): IdleObserver[K, D, Δ, Val] =
      IdleObserver(id, ev.flip.subst[SeqHandler[Token, K[Unit], D, Δ, ?]](handler))
  }
  object BlockedIdleObserver {
    def apply[D, Δ[_, _], D0](id: ObserverId): BlockedIdleObserver[D, Δ, D0, D0] = BlockedIdleObserver(id, Leibniz.refl[D0])
  }
  final case class BlockedPendingObserver[D, Δ[_, _], Val, D0](id: ObserverId, delta: Δ[D0, Val]) {
    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): BlockedPendingObserver[D, Δ, Val1, D0] =
      BlockedPendingObserver(id, dom.composeDeltas(δ, delta))
    def resume[K[_]](handler: SeqHandler[Token, K[Unit], D, Δ, D0]): PendingObserver.Aux[K, D, Δ, D0, Val] =
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

  def getValue: Option[Value] = Some(value)

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

  def observe(self: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Delta]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Option[ObserverId], Option[K[Unit]]) = {
    import SeqTrigger._
    f.handle(value) match {
      case Discard() => (this.aux1, None, None)
      case Fire(k)   => (this.aux1, None, Some(k))
      case Sleep(h)  =>
        val (cell, oid) = addObserver(h)
        (cell, Some(oid), None)
      case FireReload(k, h) =>
        val (cell, oid) = addObserver(h)
        (cell, Some(oid), Some(k))
      case Reconsider(cont) =>
        val (cell, token, oid) = block0
        (cell, Some(oid), Some(cont(self, token)))
    }
  }

  def hold(f: (D, Token[D], ObserverId) => K[Unit]): (SimpleCell.Aux1[K, D, Update, Delta, Value], ObserverId, Option[K[Unit]]) = {
    val (cell, token, oid) = block0
    val k = f(value, token, oid)
    (cell, oid, Some(k))
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

  def resume[D0 <: D](self: CellId[D], token: Token[D0], trigger: Trigger[D0]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
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
        val k = f(self, nextToken)
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

  def triggerPendingObservers(self: CellId[D]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
    @tailrec def go(
      pending: List[PendingObserver[Value]],
      idleAcc: List[IdleObserver[Value]],
      blockedIdleAcc: KMap[Token, BlockedIdleObserver[Value, ?]],
      firedAcc: Lst[K[Unit]],
      lastToken: Token[_]
    ): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) =
    pending match {
      case Nil => (SimpleCell[K, D, Update, Delta, Value](value)(idleAcc, Nil, blockedIdleAcc, blockedPendingObservers, lastObserverId, lastToken), firedAcc)
      case po :: tail =>
        import SeqTrigger._
        po.handler.handle(value, po.delta) match {
          case Discard() => go(tail, idleAcc, blockedIdleAcc, firedAcc, lastToken)
          case Fire(k) => go(tail, idleAcc, blockedIdleAcc, k :: firedAcc, lastToken)
          case Sleep(h) => go(tail, IdleObserver(po.id, h) :: idleAcc, blockedIdleAcc, firedAcc, lastToken)
          case FireReload(k, h) => go(tail, IdleObserver(po.id, h) :: idleAcc, blockedIdleAcc, k :: firedAcc, lastToken)
          case Reconsider(f) =>
            val token = lastToken.inc[Value]
            val k = f(self, token)
            go(tail, idleAcc, blockedIdleAcc.put(token)(BlockedIdleObserver(po.id, Leibniz.refl[Value])), k :: firedAcc, token)
        }
    }

    go(pendingObservers, idleObservers, blockedIdleObservers, Lst.empty, lastToken)
  }
}

private[nutcracker] object SimpleCell {
  import Cell._

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

private[nutcracker] sealed abstract class OnDemandCell[K[_], D, U, Δ[_, _]] extends Cell[K, D] {
  type Update = U
  type Delta[D0, D1] = Δ[D0, D1]

  def supply[D0 <: D](self: CellId[D], cycle: LiveCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]])
  def exclUpdate(cycle: LiveCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[Cell[K, D]]
  def addFinalizer(cycle: LiveCycle[D], sub: Subscription[K]): (Lst[K[Unit]], Cell[K, D], Option[FinalizerId])
  def removeFinalizer(cycle: LiveCycle[D], fid: FinalizerId): Cell[K, D]
}

private[nutcracker] case class InactiveCell[K[_], D, U, Δ[_, _]](
  setup: LiveCycle[D] => K[Unit],
  cycle: SporeCycle[D],
  lastObserverId: ObserverId,
  lastToken: Token[_]
) extends OnDemandCell[K, D, U, Δ] {
  type Value = Nothing

  override def getValue: Option[Value] = None

  override def hasPendingObservers: Boolean = false

  override def observe(self: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Delta]): (Cell[K, D], Option[ObserverId], Option[K[Unit]]) = {
    val newCycle = cycle.inc
    val obsId = lastObserverId.inc
    val cell = InitializingCell[K, D, U, Δ](setup, newCycle, List((obsId, f)), List(), obsId, lastToken)
    (cell, Some(obsId), Some(setup(newCycle)))
  }

  override def hold(f: (D, Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]]) = {
    val newCycle = cycle.inc
    val obsId = lastObserverId.inc
    val cell = InitializingCell[K, D, U, Δ](setup, newCycle, Nil, (obsId, f) :: Nil, obsId, lastToken)
    (cell, obsId, Some(setup(newCycle)))
  }

  override def supply[D0 <: D](self: CellId[D], cycle: LiveCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) = {
    // Must have lost all observers before it was initialized. Do nothing.
    assert(cycle.value <= this.cycle.value)
    (this, Lst.empty)
  }

  override def exclUpdate(cycle: LiveCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[Cell[K, D]] = {
    // Must be an update from a previous cell cycle. Do nothing
    assert(cycle.value <= this.cycle.value)
    None
  }

  override def resume[D0 <: D](self: CellId[D], token: Token[D0], trigger: Trigger[D0]): (Cell.Aux[K, D, U, Δ], Lst[K[Unit]]) =
  // must be resumption of an observer that has been unsubscribed already
    (this, Lst.empty)

  override def rmObserver(oid: ObserverId): (Cell[K, D], Lst[K[Unit]]) =
    // must be removing an observer that's already gone (unsubscribed or fired/discarded)
    (this, Lst.empty)

  override def triggerPendingObservers(self: CellId[D]): (Cell[K, D], Lst[K[Unit]]) =
    // there are no pending observers
    (this, Lst.empty)

  override def addFinalizer(cycle: LiveCycle[D], sub: Subscription[K]): (Lst[K[Unit]], Cell[K, D], Option[FinalizerId]) =
    (sub.unsubscribe, this, None)

  override def removeFinalizer(cycle: LiveCycle[D], fid: FinalizerId): Cell[K, D] =
    this
}

private[nutcracker] object InactiveCell {
  def init[K[_], D, U, Δ[_, _]](setup: LiveCycle[D] => K[Unit]): InactiveCell[K, D, U, Δ] =
    InactiveCell(setup, CellCycle.zero, ObserverId.zero, Token.zero)
}

private[nutcracker] case class InitializingCell[K[_], D, U, Δ[_, _]](
  setup: LiveCycle[D] => K[Unit],
  cycle: LiveCycle[D],
  preHandlers: List[(ObserverId, SeqPreHandler[Token, K[Unit], D, Δ])],
  preHandlersM: List[(ObserverId, (D, Token[D], ObserverId) => K[Unit])],
  lastObserverId: ObserverId,
  lastToken: Token[_]
) extends OnDemandCell[K, D, U, Δ] {
  import Cell._

  type Value = Nothing

  require(preHandlers.nonEmpty || preHandlersM.nonEmpty)

  private val observerCount = preHandlers.size + preHandlersM.size

  override def getValue: Option[Value] = None

  override def hasPendingObservers: Boolean = false

  override def observe(self: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Δ]): (Cell[K, D], Option[ObserverId], Option[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = copy[K, D, U, Δ](preHandlers = (obsId, f) :: preHandlers, lastObserverId = obsId)
    (cell, Some(obsId), None)
  }

  override def hold(f: (D, Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = copy[K, D, U, Δ](preHandlersM = (obsId, f) :: preHandlersM, lastObserverId = obsId)
    (cell, obsId, None)
  }

  override def supply[D0 <: D](self: CellId[D], cycle: LiveCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) = {
    if(cycle === this.cycle) {
      import SeqTrigger._

      var idleObservers: List[IdleObserver[D0]] = Nil
      var blockedIdleObservers: KMap[Token, BlockedIdleObserver[D0, ?]] = KMap()
      var ks: Lst[K[Unit]] = Lst.empty
      var token: Token[_] = lastToken

      preHandlers.foreach({ case (id, h) => h.handle(value) match {
        case Discard() => // do nothing
        case Fire(k) => ks = k :: ks
        case Sleep(handler) => idleObservers = IdleObserver(id, handler) :: idleObservers
        case FireReload(k, handler) =>
          ks = k :: ks
          idleObservers = IdleObserver(id, handler) :: idleObservers
        case Reconsider(cont) =>
          val tok = token.inc[D0]
          ks = cont(self, tok) :: ks
          blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver(id))
          token = tok
      }})

      preHandlersM.foreach({ case (id, f) => {
        val tok = token.inc[D0]
        ks = f(value, tok, id) :: ks
        blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver(id))
        token = tok
      }})

      val cell = new ActiveCell(setup, cycle, Map(), FinalizerId.zero, lastObserverId, token, value, idleObservers, blockedIdleObservers)
      (cell, ks)
    } else {
      // supplying value for a previous cycle, ignore
      assert(cycle.value <= this.cycle.value)
      (this, Lst.empty)
    }
  }

  override def exclUpdate(cycle: LiveCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[Cell[K, D]] = {
    assert(cycle.value < this.cycle.value, "Cannot have an update in a cycle whose initialization hasn't finished")
    None
  }

  override def addFinalizer(cycle: LiveCycle[D], sub: Subscription[K]): (Lst[K[Unit]], Cell[K, D], Option[FinalizerId]) =
    if(cycle === this.cycle) sys.error("Unreachable code: no one has access to the current cycle yet")
    else (sub.unsubscribe, this, None)

  override def removeFinalizer(cycle: LiveCycle[D], fid: FinalizerId): Cell[K, D] =
    if(cycle === this.cycle) sys.error("Unreachable code: no one has access to the current cycle yet")
    else this

  override def resume[D0 <: D](self: CellId[D], token: Token[D0], trigger: Trigger[D0]): (Cell.Aux[K, D, U, Δ], Lst[K[Unit]]) =
    // must be resumption of an observer that has been unsubscribed already
    (this, Lst.empty)

  override def triggerPendingObservers(self: CellId[D]): (Cell[K, D], Lst[K[Unit]]) =
    // there are no pending observers
    (this, Lst.empty)

  override def rmObserver(oid: ObserverId): (Cell[K, D], Lst[K[Unit]]) = {
    val cell = listRemoveFirst(preHandlers)(_._1 === oid) match {
      case Some(preHandlers) =>
        if(observerCount == 1) InactiveCell(setup, cycle.inc, lastObserverId, lastToken)
        else copy(preHandlers = preHandlers)
      case None => listRemoveFirst(preHandlersM)(_._1 === oid) match {
        case Some(preHandlersM) =>
          if(observerCount == 1) InactiveCell(setup, cycle.inc, lastObserverId, lastToken)
          else copy(preHandlersM = preHandlersM)
        case None => sys.error("Non-existent observer (probably trying to remove an observer twice)")
      }
    }
    (cell, Lst.empty)
  }
}

private[nutcracker] case class ActiveCell[K[_], D, U, Δ[_, _], Val <: D](
  setup: LiveCycle[D] => K[Unit],
  cycle: LiveCycle[D],
  impl: SimpleCell.Aux1[K, D, U, Δ, Val],
  finalizers: Map[FinalizerId, Subscription[K]],
  lastFinalizerId: FinalizerId
) extends OnDemandCell[K, D, U, Δ] {
  type Value = Val

  require(impl.hasObserver)

  def this(
    setup: LiveCycle[D] => K[Unit],
    cycle: LiveCycle[D],
    finalizers: Map[FinalizerId, Subscription[K]],
    lastFinalizerId: FinalizerId,
    lastObserverId: ObserverId,
    lastToken: Token[_],
    value: Val,
    idleObservers: List[Cell.IdleObserver[K, D, Δ, Val]],
    blockedIdleObservers: KMap[Token, Cell.BlockedIdleObserver[D, Δ, Val, ?]]
  ) = this(
    setup,
    cycle,
    SimpleCell[K, D, U, Δ, Val](value)(idleObservers0 = idleObservers, blockedIdleObservers0 = blockedIdleObservers, lastObsId = lastObserverId, lastTok = lastToken),
    finalizers,
    lastFinalizerId
  )

  override def getValue: Option[Value] = impl.getValue

  override def hasPendingObservers: Boolean = impl.hasPendingObservers

  override def observe(self: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Δ]): (Cell[K, D], Option[ObserverId], Option[K[Unit]]) = {
    val (cell, oid, ko) = impl.observe(self, f)
    (copy(impl = cell), oid, ko)
  }

  override def hold(f: (D, Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]]) = {
    val (cell, oid, ko) = impl.hold(f)
    (copy(impl = cell), oid, ko)
  }

  override def resume[D0 <: D](self: CellId[D], token: Token[D0], trigger: Trigger[D0]): (Cell[K, D], Lst[K[Unit]]) = {
    val (cell, ks) = impl.resume(self, token, trigger)
    if(cell.hasObserver) (copy(impl = cell), ks)
    else (InactiveCell(setup, cycle.inc, cell.lastObserverId, cell.lastToken), ks ++ collectFinalizers)
  }

  override def rmObserver(oid: ObserverId): (Cell[K, D], Lst[K[Unit]]) = {
    val cell = impl.rmObserver0(oid)
    if(cell.hasObserver) (copy(impl = cell), Lst.empty)
    else (InactiveCell(setup, cycle.inc, cell.lastObserverId, cell.lastToken), collectFinalizers)
  }

  override def supply[D0 <: D](self: CellId[D], cycle: LiveCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) = {
    if(cycle === this.cycle) sys.error("trying to initialize a cell twice in the same cell cycle")
    else {
      assert(cycle.value <= this.cycle.value)
      // Some previous cycle must have lost all observers before it was initialized. Do nothing.
      (this, Lst.empty)
    }
  }

  override def triggerPendingObservers(self: CellId[D]): (Cell[K, D], Lst[K[Unit]]) = {
    val (cell, ks) = impl.triggerPendingObservers(self)
    if(cell.hasObserver) (copy(impl = cell), ks)
    else (InactiveCell(setup, cycle.inc, cell.lastObserverId, cell.lastToken), ks ++ collectFinalizers)
  }

  override def exclUpdate(cycle: LiveCycle[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[Cell[K, D]] = {
    if(cycle === this.cycle) impl.update(u) match { // linter:ignore UseOptionMapNotPatMatch
      case Some(cell) => Some(ActiveCell[K, D, cell.Update, cell.Delta, cell.Value](setup, cycle, cell, finalizers, lastFinalizerId))
      case None => None
    } else {
      assert(cycle.value <= this.cycle.value)
      None
    }
  }

  override def addFinalizer(cycle: LiveCycle[D], sub: Subscription[K]): (Lst[K[Unit]], Cell[K, D], Option[FinalizerId]) =
    if(cycle === this.cycle) {
      val fid = lastFinalizerId.inc
      (Lst.empty, copy(finalizers = finalizers.updated(fid, sub), lastFinalizerId = fid), Some(fid))
    } else {
      (sub.unsubscribe, this, None)
    }

  override def removeFinalizer(cycle: LiveCycle[D], fid: FinalizerId): Cell[K, D] =
    if(cycle === this.cycle) copy(finalizers = finalizers - fid)
    else this

  private def collectFinalizers: Lst[K[Unit]] =
    finalizers.valuesIterator.foldLeft(Lst.empty[K[Unit]])((ks, sub) => sub.unsubscribe ++ ks)
}

private[nutcracker] sealed abstract class CellId[D] private(val domainId: Long) {
  type Domain = D
  type Update
  type Delta[_, _]

  /** Infer `Update` and `Delta` types. Relies on global uniqueness
    * of `Dom[D]` instances.
    */
  def infer(implicit dom: IDom[D]): CellId.Aux[D, dom.Update, dom.IDelta] =
    this.asInstanceOf[CellId.Aux[D, dom.Update, dom.IDelta]]

  def aux: CellId.Aux[Domain, Update, Delta] = this

  def inc[B](implicit dom: IDom[B]): CellId[B] =
    CellId[B, dom.Update, dom.IDelta](domainId + 1)
}

private[nutcracker] object CellId {
  type Aux[D, U, Δ[_, _]] = CellId[D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  val zero: CellId[Nothing] = CellId[Nothing, Nothing, Nothing](0)

  private[CellId] def apply[D, U, Δ[_, _]](domainId: Long): CellId.Aux[D, U, Δ] =
    new CellId[D](domainId) {
      type Update = U
      type Delta[D1, D2] = Δ[D1, D2]
    }

  implicit def equalInstance[D]: Equal[CellId[D]] = new Equal[CellId[D]] {
    def equal(r1: CellId[D], r2: CellId[D]): Boolean = r1.domainId == r2.domainId
  }

  implicit val equalKInstance: HEqualK[CellId] = new HEqualK[CellId] {
    def hEqual[A, B](f1: CellId[A], f2: CellId[B]): Boolean = f1.domainId == f2.domainId
  }

  implicit val orderKInstance: HOrderK[CellId] = new HOrderK[CellId] {
    override def hOrder[A, B](fa: CellId[A], fb: CellId[B]): Ordering =
      if(fa.domainId < fb.domainId) Ordering.LT
      else if(fa.domainId == fb.domainId) Ordering.EQ
      else Ordering.GT
  }

  implicit def showInstance[D]: Show[CellId[D]] = new Show[CellId[D]] {
    override def shows(ref: CellId[D]): String = s"ref${ref.domainId}"
  }

  implicit def showKInstance: ShowK[CellId] = new ShowK[CellId] {
    def shows[A](ref: CellId[A]): String = s"ref${ref.domainId}"
  }
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

private[nutcracker] object CellCycle {

  final class LiveCycle[D] private[CellCycle](val value: Long) extends AnyVal {
    def inc: SporeCycle[D] = new SporeCycle[D](value + 1)
  }

  object LiveCycle {
    implicit def equalInstance[D]: Equal[LiveCycle[D]] = new Equal[LiveCycle[D]] {
      def equal(a1: LiveCycle[D], a2: LiveCycle[D]): Boolean = a1.value == a2.value
    }
  }

  final class SporeCycle[D] private[CellCycle](val value: Long) extends AnyVal {
    def inc: LiveCycle[D] = new LiveCycle[D](value + 1)
  }

  def zero[D]: SporeCycle[D] = new SporeCycle(0)

}