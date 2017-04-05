package nutcracker

import nutcracker.util.{FreeK, HEqualK, HOrderK, Index, InjectK, KMap, KMapB, Lst, Mediated, ShowK, StateInterpreter, Step, Uncons, WriterState, `Forall{(* -> *) -> *}`, ∃}
import scala.annotation.tailrec
import scalaz.{Bind, Equal, Leibniz, Monad, Ordering, Show, StateT, Value, ~>}
import scalaz.std.option._
import scalaz.Leibniz.===
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import shapeless.{HList, Nat, Sized}

private[nutcracker] object PropagationImpl extends PersistentPropagationModule with PropagationBundle { self =>
  type Ref[A] = CellId[A]
  type Lang[K[_], A] = PropagationLang[Ref, K, A]
  type State[K[_]] = PropagationStore[K]

  implicit val refEquality: HEqualK[Ref] = CellId.equalKInstance
  implicit def refOrder: HOrderK[Ref] = CellId.orderKInstance
  implicit def refShow: ShowK[Ref] = CellId.showKInstance
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] =
    PropagationLang.freePropagation[Ref, F]

  val propagationApi: Propagation[Prg, Ref] =
    PropagationLang.freePropagation[Ref, Lang]

  def empty[K[_]]: PropagationStore[K] =
    PropagationStore[K](
      nextId = 0L,
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
    new StateInterpreter[PropagationLang[Ref, ?[_], ?], PropagationStore] {

      def step: Step[PropagationLang[Ref, ?[_], ?], PropagationStore] =
        new Step[PropagationLang[Ref, ?[_], ?], PropagationStore] {
          import PropagationLang._
          override def apply[K[_]: Monad, A](p: PropagationLang[Ref, K, A]): WriterState[Lst[K[Unit]], PropagationStore[K], A] = WriterState(s =>
            p match {
              case NewCell(d, dom) => s.addVariable(d)(dom) match {
                case (s1, ref) => (Lst.empty, s1, ref)
              }
              case Observe(ref, f, dom) => s.addDomainObserver[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom) match {
                case (s1, oo, ko) => (Lst.maybe(ko), s1, oo)
              }
              case Hold(ref, f, supply, bnd, dom) =>
                val (s1, oid, ko) = s.hold[dom.Domain, dom.Update, dom.IDelta](ref)(f)(supply)(bnd, dom)
                (Lst.maybe(ko), s1, Value(oid))
              case Supply(ref, cycle, value) =>
                val (s1, ks) = s.supply(ref)(cycle, value)
                (ks, s1, ())
              case r @ Resume(ref, token, handler, dom) =>
                (Lst.empty, s.resume[dom.Domain, dom.Update, dom.IDelta, r.Arg](ref, token, handler)(dom), ())
              case t @ Triggered(ref, token, trigger, dom) =>
                val (s1, ks) = s.triggered[dom.Domain, dom.Update, dom.IDelta, t.Arg](ref, token, trigger)(dom)
                (ks, s1, ())
              case RmObserver(ref, oid) =>
                (Lst.empty, s.rmObserver(ref, oid), ())
              case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
                case (s1, ok) => (Lst.maybe(ok), s1, ())
              }
              case Update(ref, u, dom) => (Lst.empty, s.update[dom.Domain, dom.Update, dom.IDelta](ref, u)(dom), ())
            }
          )
        }

      def uncons: Uncons[PropagationStore] = Uncons[PropagationStore](
        new `Forall{(* -> *) -> *}`[λ[K[_] => StateT[Option, PropagationStore[K], Lst[K[Unit]]]]] {
          override def compute[K[_]]: StateT[Option, PropagationStore[K], Lst[K[Unit]]] =
            StateT(_.uncons)
        })
    }

  def fetch[K[_], D](ref: Ref[D], s: State[K]): D =
    s.fetch(ref)

  def isConsistent[K[_]](s: PropagationStore[K]): Boolean =
    s.failedVars.isEmpty

  def stashable: StashPropagationModule { type Ref[A] = self.Ref[A]; type Lang[K[_], A] = self.Lang[K, A] } =
    new PropagationListModule[self.Ref, self.Lang, self.State](this)
}


private[nutcracker] case class PropagationStore[K[_]] private(
  nextId: Long,
  domains: KMap[CellId, Cell[K, ?]],
  selTriggers: KMapB[λ[`L <: HList` => Sel[CellId, L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList],
  cellsToSels: Index[CellId[_], Sel[CellId, _ <: HList]],
  failedVars: Set[Long],
  dirtyDomains: Set[CellId[_]],
  dirtySelections: Set[Sel[CellId, _ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: CellId ~> shapeless.Id = new (CellId ~> shapeless.Id) {
    def apply[D](cell: CellId[D]): D = fetch(cell)
  }

  def addVariable[D](d: D)(implicit dom: IDom[D]): (PropagationStore[K], CellId[D]) = {
    val ref = CellId[D](nextId)
    val domains1 = domains.put(ref)(SimpleCell.init(d))
    val failedVars1 = if(dom.isFailed(d)) failedVars + nextId else failedVars
    (copy(nextId = nextId + 1, domains = domains1, failedVars = failedVars1), ref)
  }

  def tryFetch[D](ref: CellId[D]): Option[D] = domains(ref).getValue

  // unsafe, should only be allowed on simple cells
  def fetch[D](ref: CellId[D]): D = tryFetch(ref).get

  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellId[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def update[D, U, Δ[_, _]](ref: CellId[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] =
    domains(ref).infer.update(u) match {
      case None => this
      case Some(cell) =>
        val failedVars1 = if(cell.getValue.fold(false)(dom.isFailed(_))) failedVars + ref.domainId else failedVars
        val domains1 = domains.put(ref.infer)(cell)
        val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
        copy(domains = domains1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }

  def addDomainObserver[D, U, Δ[_, _]](ref: CellId[D], f: SeqPreHandler[Token, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Option[ObserverId], Option[K[Unit]]) = {
    val (cell, oid, ko) = domains(ref).infer.observe(f)
    (copy(domains = domains.put(ref)(cell)), oid, ko)
  }

  def hold[D, U, Δ[_, _]](ref: CellId[D])(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K], dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], ObserverId, Option[K[Unit]]) = {
    val (cell, obsId, ko) = domains(ref).infer.hold(f)(supply)
    (copy(domains = domains.put(ref)(cell)), obsId, ko)
  }

  def supply[D](ref: CellId[D])(cycle: CellCycle[D], value: D): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = domains(ref).supply(cycle, value)
    (copy(domains = domains.put(ref)(cell)), ks)
  }

  def resume[D, U, Δ[_, _], D0 <: D](ref: CellId[D], token: Token[D0], handler: SeqHandler[Token, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] = {
    val cell = domains(ref).infer.resume(token, handler)
    val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
    copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains1)
  }

  def triggered[D, U, Δ[_, _], D0 <: D](ref: CellId[D], token: Token[D0], trigger: SeqTrigger[Token, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Lst[K[Unit]]) = {
    val (cell, ks) = domains(ref).infer.triggered(token, trigger)
    val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
    (copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains1), ks)
  }

  def rmObserver[D](ref: CellId[D], oid: ObserverId): PropagationStore[K] = {
    val cell = domains(ref).rmObserver(oid)
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
      val (cell, ks) = domains(ref).triggerPendingObservers
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

  def hasPendingObservers: Boolean

  def getValue: Option[Value]

  def update(u: Update)(implicit dom: IDom.Aux[D, Update, Delta]): Option[Cell[K, D]]

  def observe(f: SeqPreHandler[Token, K[Unit], D, Delta]): (Cell[K, D], Option[ObserverId], Option[K[Unit]])

  def rmObserver(oid: ObserverId): Cell[K, D]

  /** Making observer ID available both to the callback `f` and as part of the result, leaving the choice of how to consume it to the user. */
  def hold(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K]): (Cell[K, D], ObserverId, Option[K[Unit]])

  def supply[D0 <: D](cycle: CellCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]])

  def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): Cell.Aux[K, D, Update, Delta]

  def triggered[D0 <: D](token: Token[D0], trigger: Trigger[D0]): (Cell[K, D], Lst[K[Unit]])

  def triggerPendingObservers: (Cell[K, D], Lst[K[Unit]])
}

private[nutcracker] object Cell {
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
  final case class BlockedIdleObserver[D, Δ[_, _], D0, Val](id: ObserverId, ev: Val === D0) {
    def addDelta[Val1](δ: Δ[Val, Val1]): BlockedPendingObserver[D, Δ, D0, Val1] =
      BlockedPendingObserver(id, ev.subst[Δ[?, Val1]](δ))
    def resume[K[_]](handler: SeqHandler[Token, K[Unit], D, Δ, D0]): IdleObserver[K, D, Δ, Val] =
      IdleObserver(id, ev.flip.subst[SeqHandler[Token, K[Unit], D, Δ, ?]](handler))
  }
  object BlockedIdleObserver {
    def apply[D, Δ[_, _], D0](id: ObserverId): BlockedIdleObserver[D, Δ, D0, D0] = BlockedIdleObserver(id, Leibniz.refl[D0])
  }
  final case class BlockedPendingObserver[D, Δ[_, _], D0, Val](id: ObserverId, delta: Δ[D0, Val]) {
    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): BlockedPendingObserver[D, Δ, D0, Val1] =
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
  val blockedIdleObservers: KMap[Token, BlockedIdleObserver[?, Value]]
  val blockedPendingObservers: KMap[Token, BlockedPendingObserver[?, Value]]

  val lastObserverId: ObserverId
  val lastToken: Token[_]

  def aux1: SimpleCell.Aux1[K, D, Update, Delta, Value] = this

  def copy(
    idleObservers: List[IdleObserver[Value]] = idleObservers,
    pendingObservers: List[PendingObserver[Value]] = pendingObservers,
    blockedIdleObservers: KMap[Token, BlockedIdleObserver[?, Value]] = blockedIdleObservers,
    blockedPendingObservers: KMap[Token, BlockedPendingObserver[?, Value]] = blockedPendingObservers,
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

        val blocked0 = blockedPendingObservers.mapValues[BlockedPendingObserver[?, up.NewValue]](
          λ[BlockedPendingObserver[?, Value] ~> BlockedPendingObserver[?, up.NewValue]](_.addDelta(delta))
        )
        val blocked1 = blockedIdleObservers.mapValues[BlockedPendingObserver[?, up.NewValue]](
          λ[BlockedIdleObserver[?, Value] ~> BlockedPendingObserver[?, up.NewValue]](_.addDelta(delta))
        )
        val blocked = blocked0 ++ blocked1

        Some(SimpleCell(newVal)(Nil, pending, KMap[Token, BlockedIdleObserver[?, up.NewValue]](), blocked, lastObserverId, lastToken))

      case Unchanged() => None
    }

  def observe(f: SeqPreHandler[Token, K[Unit], D, Delta]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Option[ObserverId], Option[K[Unit]]) = {
    import SeqTrigger._
    f.handle(value) match {
      case Discard() => (this.aux1, None, None)
      case Fire(k)   => (this.aux1, None, Some(k))
      case Sleep(h)  =>
        val (cell, oid) = addObserver(h)
        (cell, Some(oid), None)
      case FireReload(cont) =>
        val (cell, token, oid) = block0
        (cell, Some(oid), Some(cont(token)))
    }
  }

  def hold(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K]): (SimpleCell.Aux1[K, D, Update, Delta, Value], ObserverId, Option[K[Unit]]) = {
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

  def rmObserver(oid: ObserverId): SimpleCell.Aux1[K, D, Update, Delta, Value] = {
    listRemoveFirst(idleObservers)(_.id === oid) match {
      case Some(idles) => copy(idleObservers = idles)
      case None => listRemoveFirst(pendingObservers)(_.id === oid) match {
        case Some(pendings) => copy(pendingObservers = pendings)
        case None => mapRemoveFirst[Token, BlockedIdleObserver[?, Value]](blockedIdleObservers)(_.id === oid) match {
          case Some(blockedIdles) => copy(blockedIdleObservers = blockedIdles)
          case None => mapRemoveFirst[Token, BlockedPendingObserver[?, Value]](blockedPendingObservers)(_.id === oid) match {
            case Some(blockedPendings) => copy(blockedPendingObservers = blockedPendings)
            case None => this
          }
        }
      }
    }
  }

  def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): SimpleCell.Aux1[K, D, Update, Delta, Value] =
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

  def triggered[D0 <: D](token: Token[D0], trigger: Trigger[D0]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
    import SeqTrigger._
    trigger match {
      case Discard() => (copy(blockedIdleObservers = blockedIdleObservers - token, blockedPendingObservers = blockedPendingObservers - token), Lst.empty)
      case Fire(k)   => (copy(blockedIdleObservers = blockedIdleObservers - token, blockedPendingObservers = blockedPendingObservers - token), Lst.singleton(k))
      case Sleep(h)  => (resume(token, h), Lst.empty)
      case FireReload(f) =>
        val nextToken = lastToken.inc[D0]
        val k = f(nextToken)
        blockedIdleObservers.get(token) match {
          case Some(obs) =>
            assert(blockedPendingObservers.get(token).isEmpty)
            val cell = copy(
              blockedIdleObservers = (blockedIdleObservers - token).put(nextToken)(obs),
              lastToken = nextToken
            )
            (cell, Lst.singleton(k))
          case None => blockedPendingObservers.get(token) match {
            case Some(obs) =>
              val cell = copy(
                blockedPendingObservers = (blockedPendingObservers - token).put(nextToken)(obs),
                lastToken = nextToken
              )
              (cell, Lst.singleton(k))
            case None =>
              sys.error(s"unrecognized token $token")
          }
        }
    }
  }

  def triggerPendingObservers: (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
    @tailrec def go(
      pending: List[PendingObserver[Value]],
      idleAcc: List[IdleObserver[Value]],
      blockedIdleAcc: KMap[Token, BlockedIdleObserver[?, Value]],
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
          case FireReload(f) =>
            val token = lastToken.inc[Value]
            val k = f(token)
            go(tail, idleAcc, blockedIdleAcc.put(token)(BlockedIdleObserver(po.id, Leibniz.refl[Value])), k :: firedAcc, token)
        }
    }

    go(pendingObservers, idleObservers, blockedIdleObservers, Lst.empty, lastToken)
  }

  def supply[D0 <: D](cycle: CellCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) =
    sys.error("operation valid only for OnDemandCells")
}

private[nutcracker] object SimpleCell {
  import Cell._

  type Aux[K[_], D, U, Δ[_, _]] = SimpleCell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }
  type Aux1[K[_], D, U, Δ[_, _], Val] = SimpleCell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2]; type Value = Val }

  def init[K[_], D](d: D)(implicit dom: IDom[D]): SimpleCell.Aux[K, D, dom.Update, dom.IDelta] =
    SimpleCell[K, D, dom.Update, dom.IDelta, D](d)(
      Nil,
      Nil,
      KMap[Token, BlockedIdleObserver[D, dom.IDelta, ?, D]](),
      KMap[Token, BlockedPendingObserver[D, dom.IDelta, ?, D]](),
      ObserverId.zero,
      Token.zero
    )

  def apply[K[_], D, U, Δ[_, _], Val <: D](d: Val)(
    idleObservers0: List[IdleObserver[K, D, Δ, Val]],
    pendingObservers0: List[PendingObserver[K, D, Δ, Val]] = Nil,
    blockedIdleObservers0: KMap[Token, BlockedIdleObserver[D, Δ, ?, Val]],
    blockedPendingObservers0: KMap[Token, BlockedPendingObserver[D, Δ, ?, Val]] = KMap[Token, BlockedPendingObserver[D, Δ, ?, Val]](),
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

  override def hold(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]])

  final override def update(u: U)(implicit dom: IDom.Aux[D, U, Δ]): Option[Cell.Aux[K, D, U, Δ]] =
    sys.error("Cannot update OnDemandCell")
}

private[nutcracker] case class InactiveCell[K[_], D, U, Δ[_, _]](
  setup: Mediated[K, D, CellCycle[D], Unit],
  cycle: CellCycle[D],
  lastObserverId: ObserverId,
  lastToken: Token[_]
) extends OnDemandCell[K, D, U, Δ] {

  override def getValue: Option[Value] = None

  override def hasPendingObservers: Boolean = false

  override def observe(f: SeqPreHandler[Token, K[Unit], D, Delta]): (Cell[K, D], Option[ObserverId], Option[K[Unit]]) = {
    val newCycle = cycle.inc
    val obsId = lastObserverId.inc
    val cell = InitializingCell[K, D, U, Δ](setup, newCycle, List((obsId, f)), List(), obsId, lastToken)
    (cell, Some(obsId), None)
  }

  override def hold(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]]) = {
    val newCycle = cycle.inc
    val obsId = lastObserverId.inc
    val cell = InitializingCell[K, D, U, Δ](setup, newCycle, Nil, (obsId, f) :: Nil, obsId, lastToken)
    (cell, obsId, Some(setup.completeM(d => supply(newCycle, d).map((_: Unit) => newCycle))))
  }

  def supply[D0 <: D](cycle: CellCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) = {
    // Must have lost all observers before it was initialized. Do nothing.
    assert(cycle.value <= this.cycle.value)
    (this, Lst.empty)
  }

  override def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): Cell.Aux[K, D, U, Δ] =
    // must be resumption of an observer that has been unsubscribed already
    this

  override def triggered[D0 <: D](token: Token[D0], trigger: Trigger[D0]): (Cell.Aux[K, D, U, Δ], Lst[K[Unit]]) =
  // must be resumption of an observer that has been unsubscribed already
    (this, Lst.empty)

  override def rmObserver(oid: ObserverId): Cell[K, D] =
    // must be removing an observer that's already gone (unsubscribed or fired/discarded)
    this

  override def triggerPendingObservers: (Cell[K, D], Lst[K[Unit]]) =
    // there are no pending observers
    (this, Lst.empty)
}

private[nutcracker] case class InitializingCell[K[_], D, U, Δ[_, _]](
  setup: Mediated[K, D, CellCycle[D], Unit],
  cycle: CellCycle[D],
//  blockedIdleObservers: Map[Token[D], Cell.BlockedIdleObserver[D, Δ, D, D]],
  preHandlers: List[(ObserverId, SeqPreHandler[Token, K[Unit], D, Δ])],
  preHandlersM: List[(ObserverId, (D, Token[D], ObserverId) => K[Unit])],
  lastObserverId: ObserverId,
  lastToken: Token[_]
) extends OnDemandCell[K, D, U, Δ] {
  import Cell._

  require(preHandlers.nonEmpty || preHandlersM.nonEmpty)

  private val observerCount = preHandlers.size + preHandlersM.size

  override def getValue: Option[Value] = None

  override def hasPendingObservers: Boolean = false

  override def observe(f: SeqPreHandler[Token, K[Unit], D, Δ]): (Cell[K, D], Option[ObserverId], Option[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = InitializingCell[K, D, U, Δ](setup, cycle, (obsId, f) :: preHandlers, preHandlersM, obsId, lastToken)
    (cell, Some(obsId), None)
  }

  override def hold(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = InitializingCell[K, D, U, Δ](setup, cycle, preHandlers, (obsId, f) :: preHandlersM, obsId, lastToken)
    (cell, obsId, None)
  }

  override def supply[D0 <: D](cycle: CellCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) = {
    if(cycle === this.cycle) {
      import SeqTrigger._

      var idleObservers: List[IdleObserver[D0]] = Nil
      var blockedIdleObservers: KMap[Token, BlockedIdleObserver[?, D0]] = KMap[Token, BlockedIdleObserver[?, D0]]()
      var ks: Lst[K[Unit]] = Lst.empty
      var token: Token[_] = lastToken

      preHandlers.foreach({ case (id, h) => h.handle(value) match {
        case Discard() => // do nothing
        case Fire(k) => ks = k :: ks
        case Sleep(handler) => idleObservers = IdleObserver(id, handler) :: idleObservers
        case FireReload(cont) =>
          val tok = token.inc[D0]
          ks = cont(tok) :: ks
          blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver(id))
          token = tok
      }})

      preHandlersM.foreach({ case (id, f) => {
        val tok = token.inc[D0]
        ks = f(value, tok, id) :: ks
        blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver(id))
        token = tok
      }})

      val cell = new ActiveCell(setup, cycle, lastObserverId, token, value, idleObservers, blockedIdleObservers)
      (cell, ks)
    } else {
      // supplying value for a previous cycle, ignore
      assert(cycle.value <= this.cycle.value)
      (this, Lst.empty)
    }
  }

  override def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): Aux[K, D, U, Δ] =
    // must be resumption of an observer that has been unsubscribed already
    this

  override def triggered[D0 <: D](token: Token[D0], trigger: Trigger[D0]): (Cell.Aux[K, D, U, Δ], Lst[K[Unit]]) =
  // must be resumption of an observer that has been unsubscribed already
    (this, Lst.empty)

  override def triggerPendingObservers: (Cell[K, D], Lst[K[Unit]]) =
  // there are no pending observers
    (this, Lst.empty)

  override def rmObserver(oid: ObserverId): Cell[K, D] =
    listRemoveFirst(preHandlers)(_._1 === oid) match {
      case Some(preHandlers) =>
        if(observerCount == 1) InactiveCell(setup, cycle.inc, lastObserverId, lastToken)
        else               InitializingCell(setup, cycle, preHandlers, preHandlersM, lastObserverId, lastToken)
      case None => listRemoveFirst(preHandlersM)(_._1 === oid) match {
        case Some(preHandlersM) =>
          if(observerCount == 1) InactiveCell(setup, cycle.inc, lastObserverId, lastToken)
          else               InitializingCell(setup, cycle, preHandlers, preHandlersM, lastObserverId, lastToken)
        case None => sys.error("Non-existent observer (probably trying to remove an observer twice)")
      }
    }
}

private[nutcracker] case class ActiveCell[K[_], D, U, Δ[_, _], Val <: D](
  setup: Mediated[K, D, CellCycle[D], Unit],
  cycle: CellCycle[D],
  impl: SimpleCell.Aux1[K, D, U, Δ, Val]
) extends OnDemandCell[K, D, U, Δ] {
  type Value = Val

  require(impl.hasObserver)

  def this(
    setup: Mediated[K, D, CellCycle[D], Unit],
    cycle: CellCycle[D],
    lastObserverId: ObserverId,
    lastToken: Token[_],
    value: Val,
    idleObservers: List[Cell.IdleObserver[K, D, Δ, Val]],
    blockedIdleObservers: KMap[Token, Cell.BlockedIdleObserver[D, Δ, ?, Val]]
  ) = this(setup, cycle, SimpleCell[K, D, U, Δ, Val](value)(idleObservers0 = idleObservers, blockedIdleObservers0 = blockedIdleObservers, lastObsId = lastObserverId, lastTok = lastToken))

  override def getValue: Option[Value] = impl.getValue

  override def hasPendingObservers: Boolean = impl.hasPendingObservers

  override def observe(f: SeqPreHandler[Token, K[Unit], D, Δ]): (Cell[K, D], Option[ObserverId], Option[K[Unit]]) = {
    val (cell, oid, ko) = impl.observe(f)
    (copy(impl = cell), oid, ko)
  }

  override def hold(f: (D, Token[D], ObserverId) => K[Unit])(supply: (CellCycle[D], D) => K[Unit])(implicit K: Bind[K]): (OnDemandCell[K, D, U, Δ], ObserverId, Option[K[Unit]]) = {
    val (cell, oid, ko) = impl.hold(f)(supply)
    (copy(impl = cell), oid, ko)
  }

  override def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): Cell.Aux[K, D, U, Δ] = {
    val cell = impl.resume(token, handler)
    copy[K, D, U, Δ, Val](impl = cell)
  }

  override def triggered[D0 <: D](token: Token[D0], trigger: Trigger[D0]): (Cell[K, D], Lst[K[Unit]]) = {
    val (cell, ks) = impl.triggered(token, trigger)
    if(cell.hasObserver) (copy(impl = cell), ks)
    else (InactiveCell(setup, cycle.inc, cell.lastObserverId, cell.lastToken), ks)
  }

  override def rmObserver(oid: ObserverId): Cell[K, D] = {
    val cell = impl.rmObserver(oid)
    if(cell.hasObserver) copy(impl = cell)
    else InactiveCell(setup, cycle.inc, cell.lastObserverId, cell.lastToken)
  }

  override def supply[D0 <: D](cycle: CellCycle[D], value: D0): (Cell[K, D], Lst[K[Unit]]) = {
    if(cycle === this.cycle) sys.error("trying to initialize a cell twice in the same cell cycle")
    else {
      assert(cycle.value <= this.cycle.value)
      // Some previous cycle must have lost all observers before it was initialized. Do nothing.
      (this, Lst.empty)
    }
  }

  override def triggerPendingObservers: (Cell[K, D], Lst[K[Unit]]) = {
    val (cell, ks) = impl.triggerPendingObservers
    if(cell.hasObserver) (copy(impl = cell), ks)
    else (InactiveCell(setup, cycle.inc, cell.lastObserverId, cell.lastToken), ks)
  }
}

private[nutcracker] sealed abstract class CellId[D](val domainId: Long) {
  type Domain = D
  type Update
  type Delta[_, _]

  /** Infer `Update` and `Delta` types. Relies on global uniqueness
    * of `Dom[D]` instances.
    */
  def infer(implicit dom: IDom[D]): CellId.Aux[D, dom.Update, dom.IDelta] =
    this.asInstanceOf[CellId.Aux[D, dom.Update, dom.IDelta]]

  def aux: CellId.Aux[Domain, Update, Delta] = this
}

private[nutcracker] object CellId {
  type Aux[D, U, Δ[_, _]] = CellId[D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  def apply[D](domainId: Long)(implicit dom: IDom[D]): CellId.Aux[D, dom.Update, dom.IDelta] =
    new CellId[D](domainId) {
      type Update = dom.Update
      type Delta[D1, D2] = dom.IDelta[D1, D2]
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

private[nutcracker] final case class CellCycle[D](val value: Long) extends AnyVal {
  def inc: CellCycle[D] = CellCycle(value + 1)
}
private[nutcracker] object CellCycle {
  implicit def equalInstance[D]: Equal[CellCycle[D]] = new Equal[CellCycle[D]] {
    def equal(a1: CellCycle[D], a2: CellCycle[D]): Boolean = a1.value == a2.value
  }
}