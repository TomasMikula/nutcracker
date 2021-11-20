package nutcracker.toolkit

import nutcracker.util.{KMap, Lst, ∃}
import nutcracker.{IDom, SeqHandler, SeqPreHandler, SeqTrigger, Subscription}
import scala.annotation.tailrec
import scalaz.Leibniz.===
import scalaz.syntax.equal._
import scalaz.{Equal, Leibniz, \/, ~>}


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
}

private[nutcracker] object Cell {
  type AuxD[K[_], D, Δ[_, _]] = Cell[K, D] { type Delta[D1, D2] = Δ[D1, D2] }
  type Aux[K[_], D, U, Δ[_, _]] = Cell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  type IncarnationId[K[_], D] = SimpleCellId[K, D] \/ (AutoCellId[K, D], CellCycle[D])
  type Tok[K[_], D, D0] = (IncarnationId[K, D], Token[D0])

  final case class IdleObserver[K[_], D, Δ[_, _], Val](id: ObserverId, handler: SeqHandler[Tok[K, D, *], K, D, Δ, Val]) {
    def addDelta[Val1](δ: Δ[Val, Val1]): PendingObserver.Aux[K, D, Δ, Val, Val1] =
      PendingObserver[K, D, Δ, Val, Val1](id, δ, handler)
  }
  sealed abstract class PendingObserver[K[_], D, Δ[_, _], Val](val id: ObserverId) {
    type D0
    val delta: Δ[D0, Val]
    val handler: SeqHandler[Tok[K, D, *], K, D, Δ, D0]

    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): PendingObserver.Aux[K, D, Δ, D0, Val1] =
      PendingObserver[K, D, Δ, D0, Val1](id, dom.composeDeltas(δ, delta), handler)
  }
  object PendingObserver {
    type Aux[K[_], D, Δ[_, _], From, Val] = PendingObserver[K, D, Δ, Val] { type D0 = From }

    def apply[K[_], D, Δ[_, _], From, Val](id: ObserverId, delta: Δ[From, Val], handler: SeqHandler[Tok[K, D, *], K, D, Δ, From]): Aux[K, D, Δ, From, Val] =
      new PendingObserver0(id, delta, handler)

    private final class PendingObserver0[K[_], D, Δ[_, _], From, Val](id: ObserverId, val delta: Δ[From, Val], val handler: SeqHandler[Tok[K, D, *], K, D, Δ, From]) extends PendingObserver[K, D, Δ, Val](id) {
      type D0 = From
    }
  }
  final case class BlockedIdleObserver[D, Δ[_, _], Val, D0](id: ObserverId, ev: Val === D0) {
    def addDelta[Val1](δ: Δ[Val, Val1]): BlockedPendingObserver[D, Δ, Val1, D0] =
      BlockedPendingObserver(id, ev.subst[Δ[*, Val1]](δ))
    def resume[K[_]](handler: SeqHandler[Tok[K, D, *], K, D, Δ, D0]): IdleObserver[K, D, Δ, Val] =
      IdleObserver(id, ev.flip.subst[SeqHandler[Tok[K, D, *], K, D, Δ, *]](handler))
  }
  object BlockedIdleObserver {
    def apply[D, Δ[_, _], D0](id: ObserverId): BlockedIdleObserver[D, Δ, D0, D0] = BlockedIdleObserver(id, Leibniz.refl[D0])
  }
  final case class BlockedPendingObserver[D, Δ[_, _], Val, D0](id: ObserverId, delta: Δ[D0, Val]) {
    def addDelta[Val1, U](δ: Δ[Val, Val1])(implicit dom: IDom.Aux[D, U, Δ]): BlockedPendingObserver[D, Δ, Val1, D0] =
      BlockedPendingObserver(id, dom.composeDeltas(δ, delta))
    def resume[K[_]](handler: SeqHandler[Tok[K, D, *], K, D, Δ, D0]): PendingObserver.Aux[K, D, Δ, D0, Val] =
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
  import nutcracker.{Unchanged, Updated}
  import Cell._

  val value: Value

  val idleObservers: List[IdleObserver[Value]]
  val pendingObservers: List[PendingObserver[Value]]
  val blockedIdleObservers: KMap[Token, BlockedIdleObserver[Value, *]]
  val blockedPendingObservers: KMap[Token, BlockedPendingObserver[Value, *]]

  val lastObserverId: ObserverId
  val lastToken: Token[_]


  def infer(implicit dom: IDom[D]): SimpleCell.Aux[K, D, dom.Update, dom.IDelta] =
    this.asInstanceOf[SimpleCell.Aux[K, D, dom.Update, dom.IDelta]]

  def aux1: SimpleCell.Aux1[K, D, Update, Delta, Value] = this

  def copy(
    idleObservers: List[IdleObserver[Value]] = idleObservers,
    pendingObservers: List[PendingObserver[Value]] = pendingObservers,
    blockedIdleObservers: KMap[Token, BlockedIdleObserver[Value, *]] = blockedIdleObservers,
    blockedPendingObservers: KMap[Token, BlockedPendingObserver[Value, *]] = blockedPendingObservers,
    nextObserverId: ObserverId = lastObserverId,
    lastToken: Token[_] = lastToken
  ): SimpleCell.Aux1[K, D, Update, Delta, Value] =
    SimpleCell[K, D, Update, Delta, Value](value)(idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, nextObserverId, lastToken)

  def hasObserver: Boolean = idleObservers.nonEmpty || pendingObservers.nonEmpty || blockedIdleObservers.nonEmpty || blockedPendingObservers.nonEmpty

  private def hasPendingObservers: Boolean = pendingObservers.nonEmpty

  def update(u: Update)(implicit dom: IDom.Aux[D, Update, Delta]): CellUpdateResult[SimpleCell[K, D]] =
    dom.update(value, u) match {
      case up @ Updated(newVal, delta) =>
        val pending0 = pendingObservers.map(_.addDelta(delta))
        val pending1 = idleObservers.map(_.addDelta(delta))
        val pending = pending1 ::: pending0

        val blocked0 = blockedPendingObservers.mapValues[BlockedPendingObserver[up.NewValue, *]](
          λ[BlockedPendingObserver[Value, *] ~> BlockedPendingObserver[up.NewValue, *]](_.addDelta(delta))
        )
        val blocked1 = blockedIdleObservers.mapValues[BlockedPendingObserver[up.NewValue, *]](
          λ[BlockedIdleObserver[Value, *] ~> BlockedPendingObserver[up.NewValue, *]](_.addDelta(delta))
        )
        val blocked = blocked0 ++ blocked1

        val cell = SimpleCell[K, D, Update, Delta, D](newVal)(Nil, pending, KMap[Token, BlockedIdleObserver[up.NewValue, *]](), blocked, lastObserverId, lastToken)
        val becameDirty = !this.hasPendingObservers && cell.hasPendingObservers
        CellUpdated(cell, becameDirty)

      case Unchanged() => CellUnchanged
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

  def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]], Boolean) = {
    import SeqTrigger._
    trigger match {
      case Discard() => (copy(blockedIdleObservers = blockedIdleObservers - token, blockedPendingObservers = blockedPendingObservers - token), Lst.empty, false)
      case Fire(k)   => (copy(blockedIdleObservers = blockedIdleObservers - token, blockedPendingObservers = blockedPendingObservers - token), Lst.singleton(k), false)
      case Sleep(handler) =>
        val (cell, becameDirty) = resumeWithHandler(token, handler)
        (cell, Lst.empty, becameDirty)
      case FireReload(k, handler) =>
        val (cell, becameDirty) = resumeWithHandler(token, handler)
        (cell, Lst.singleton(k), becameDirty)
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
        (cell, Lst.singleton(k), false)
    }
  }

  private def resumeWithHandler[D0 <: D](token: Token[D0], handler: Handler[D0]): (SimpleCell.Aux1[K, D, Update, Delta, Value], Boolean) =
    blockedIdleObservers.get(token) match {
      case Some(obs) =>
        assert(blockedPendingObservers.get(token).isEmpty)
        val obs1 = obs.resume(handler)
        (SimpleCell[K, D, Update, Delta, Value](value)(obs1 :: idleObservers, pendingObservers, blockedIdleObservers - token, blockedPendingObservers, lastObserverId, lastToken), false)
      case None => blockedPendingObservers.get(token) match {
        case Some(obs) =>
          val obs1 = obs.resume(handler)
          val becameDirty = pendingObservers.isEmpty
          (SimpleCell[K, D, Update, Delta, Value](value)(idleObservers, obs1 :: pendingObservers, blockedIdleObservers, blockedPendingObservers - token, lastObserverId, lastToken), becameDirty)
        case None =>
          sys.error(s"unrecognized token $token")
      }
    }

  def triggerPendingObservers(self: CellIncarnationId): (SimpleCell.Aux1[K, D, Update, Delta, Value], Lst[K[Unit]]) = {
    // @tailrec // can't use because pattern matching sucks, so we use Church encoding (fold)
    def go(
      pending: List[PendingObserver[Value]],
      idleAcc: List[IdleObserver[Value]],
      blockedIdleAcc: KMap[Token, BlockedIdleObserver[Value, *]],
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
    blockedIdleObservers0: KMap[Token, BlockedIdleObserver[D, Δ, Val, *]],
    blockedPendingObservers0: KMap[Token, BlockedPendingObserver[D, Δ, Val, *]] = KMap[Token, BlockedPendingObserver[D, Δ, Val, *]](), // https://github.com/scala/scala-dev/issues/366
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

  def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (Option[(OnDemandCell[K, D], Boolean)], Lst[K[Unit]])

  def supply[D0 <: D](self: CellIncarnationId, value: D0): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def exclUpdate(u: Update)(implicit dom: IDom.Aux[D, Update, Delta]): CellUpdateResult[OnDemandCell[K, D]]

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
    var blockedIdleObservers: KMap[Token, BlockedIdleObserver[D0, *]] = KMap()
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

  override def exclUpdate(u: U)(implicit dom: IDom.Aux[D, U, Δ]): CellUpdateResult[OnDemandCell[K, D]] = {
    sys.error("Unreachable code: no one has access to the current cycle yet")
  }

  override def addFinalizer(sub: Subscription[K]): (Lst[K[Unit]], OnDemandCell[K, D], FinalizerId) =
    sys.error("Unreachable code: no one has access to the current cycle yet")

  override def removeFinalizer(fid: FinalizerId): OnDemandCell[K, D] =
    sys.error("Unreachable code: no one has access to the current cycle yet")

  override def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (Option[(OnDemandCell.Aux[K, D, U, Δ], Boolean)], Lst[K[Unit]]) =
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
    blockedIdleObservers: KMap[Token, Cell.BlockedIdleObserver[D, Δ, Val, *]]
  ) = this(
    cycle,
    SimpleCell[K, D, U, Δ, Val](value)(idleObservers0 = idleObservers, blockedIdleObservers0 = blockedIdleObservers, lastObsId = lastObserverId, lastTok = lastToken),
    finalizers,
    lastFinalizerId
  )

  override def getValue: Option[Value] = Some(impl.value)

  override def observe(self: CellIncarnationId, f: PreHandler): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]]) = {
    val (cell, oid, ks) = impl.observe(self, f)
    (copy(impl = cell), oid, ks)
  }

  override def hold(f: (D, CellCycle[D], Token[D], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]]) = {
    val (cell, oid, ks) = impl.hold(f(_, cycle, _, _))
    (copy(impl = cell), oid, ks)
  }

  override def resume[D0 <: D](self: CellIncarnationId, token: Token[D0], trigger: Trigger[D0]): (Option[(OnDemandCell[K, D], Boolean)], Lst[K[Unit]]) = {
    val (cell, ks, becameDirty) = impl.resume(self, token, trigger)

    if (cell.hasObserver) {
      (Some((copy(impl = cell), becameDirty)), ks)
    } else {
      assert(!becameDirty)
      (None, ks ++ collectFinalizers)
    }
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

  override def exclUpdate(u: U)(implicit dom: IDom.Aux[D, U, Δ]): CellUpdateResult[OnDemandCell[K, D]] = {
    impl.update(u) match {
      case CellUpdated(cell, becameDirty) => CellUpdated(ActiveCell[K, D, cell.Update, cell.Delta, cell.Value](cycle, cell, finalizers, lastFinalizerId), becameDirty)
      case CellUnchanged => CellUnchanged
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

private[toolkit] sealed abstract class CellUpdateResult[+C]
private[toolkit] case class CellUpdated[C](cell: C, becameDirty: Boolean) extends CellUpdateResult[C]
private[toolkit] case object CellUnchanged extends CellUpdateResult[Nothing]

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