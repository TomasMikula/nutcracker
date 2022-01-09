package nutcracker.toolkit

import nutcracker.util.{Exists, KMap, Lst}
import nutcracker.{IDom, SeqHandler, SeqPreHandler, SeqTrigger, Subscription}
import scala.annotation.tailrec
import scalaz.syntax.equal._
import scalaz.{Equal, Leibniz, \/, ~>, ===}


private[nutcracker] sealed abstract class Cell[K[_], D[_]] {
  type Update[_]
  type Delta[_, _]
  type Idx

  type CellIncarnationId = SimpleCellId[K, D] \/ (AutoCellId[K, D], CellCycle[D])
  type Tok[I] = (CellIncarnationId, Token[I])
  type PreHandler = SeqPreHandler[Tok, K, D, Delta]
  type Handler[I] = SeqHandler[Tok, K, D, Delta, I]
  type Trigger[I] = SeqTrigger[Tok, K, D, Delta, I]

  type IdleObserver[I] = Cell.IdleObserver[K, D, Delta, I]
  type PendingObserver[I] = Cell.PendingObserver[K, D, Delta, I]
  type BlockedIdleObserver[I, J] = Cell.BlockedIdleObserver[D, Delta, I, J]
  type BlockedPendingObserver[I, J] = Cell.BlockedPendingObserver[D, Delta, I, J]
}

private[nutcracker] object Cell {
  type AuxD[K[_], D[_], Δ[_, _]] = Cell[K, D] { type Delta[I, J] = Δ[I, J] }
  type Aux[K[_], D[_], U, Δ[_, _]] = Cell[K, D] { type Update = U; type Delta[I, J] = Δ[I, J] }

  type IncarnationId[K[_], D[_]] = SimpleCellId[K, D] \/ (AutoCellId[K, D], CellCycle[D])
  type Tok[K[_], D[_], I] = (IncarnationId[K, D], Token[I])

  final case class IdleObserver[K[_], D[_], Δ[_, _], Val](id: ObserverId, handler: SeqHandler[Tok[K, D, *], K, D, Δ, Val]) {
    def addDelta[Val1](δ: Δ[Val, Val1]): PendingObserver.Aux[K, D, Δ, Val, Val1] =
      PendingObserver[K, D, Δ, Val, Val1](id, δ, handler)
  }
  sealed abstract class PendingObserver[K[_], D[_], Δ[_, _], I](val id: ObserverId) {
    type I0
    val delta: Δ[I0, I]
    val handler: SeqHandler[Tok[K, D, *], K, D, Δ, I0]

    def addDelta[I1, U[_]](δ: Δ[I, I1])(implicit dom: IDom.Aux[D, U, Δ]): PendingObserver.Aux[K, D, Δ, I0, I1] =
      PendingObserver[K, D, Δ, I0, I1](id, dom.composeDeltas(δ, delta), handler)
  }
  object PendingObserver {
    type Aux[K[_], D[_], Δ[_, _], From, To] = PendingObserver[K, D, Δ, To] { type I0 = From }

    def apply[K[_], D[_], Δ[_, _], From, To](id: ObserverId, delta: Δ[From, To], handler: SeqHandler[Tok[K, D, *], K, D, Δ, From]): Aux[K, D, Δ, From, To] =
      new PendingObserver0(id, delta, handler)

    private final class PendingObserver0[K[_], D[_], Δ[_, _], From, To](id: ObserverId, val delta: Δ[From, To], val handler: SeqHandler[Tok[K, D, *], K, D, Δ, From]) extends PendingObserver[K, D, Δ, To](id) {
      override type I0 = From
    }
  }
  final case class BlockedIdleObserver[D[_], Δ[_, _], I, J](id: ObserverId, ev: J === I) {
    def addDelta[J1](δ: Δ[J, J1]): BlockedPendingObserver[D, Δ, I, J1] =
      BlockedPendingObserver(id, ev.subst[Δ[*, J1]](δ))
    def resume[K[_]](handler: SeqHandler[Tok[K, D, *], K, D, Δ, I]): IdleObserver[K, D, Δ, J] =
      IdleObserver(id, ev.flip.subst[SeqHandler[Tok[K, D, *], K, D, Δ, *]](handler))
  }
  object BlockedIdleObserver {
    def apply[D[_], Δ[_, _], I](id: ObserverId): BlockedIdleObserver[D, Δ, I, I] = BlockedIdleObserver(id, Leibniz.refl[I])
  }
  final case class BlockedPendingObserver[D[_], Δ[_, _], I, J](id: ObserverId, delta: Δ[I, J]) {
    def addDelta[J1, U[_]](δ: Δ[J, J1])(implicit dom: IDom.Aux[D, U, Δ]): BlockedPendingObserver[D, Δ, I, J1] =
      BlockedPendingObserver(id, dom.composeDeltas(δ, delta))
    def resume[K[_]](handler: SeqHandler[Tok[K, D, *], K, D, Δ, I]): PendingObserver.Aux[K, D, Δ, I, J] =
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

  def mapRemoveFirst[Key[_], V[_]](m: KMap[Key, V])(p: Exists[V] => Boolean): Option[KMap[Key, V]] =
    m.find(p).map(m - _._1)
}

private[nutcracker] abstract class SimpleCell[K[_], D[_]] extends Cell[K, D] {
  import nutcracker.{Unchanged, Updated}
  import Cell._

  val value: D[Idx]

  val idleObservers: List[IdleObserver[Idx]]
  val pendingObservers: List[PendingObserver[Idx]]
  val blockedIdleObservers: KMap[Token, BlockedIdleObserver[*, Idx]]
  val blockedPendingObservers: KMap[Token, BlockedPendingObserver[*, Idx]]

  val lastObserverId: ObserverId
  val lastToken: Token[_]


  def infer(implicit dom: IDom[D]): SimpleCell.Aux[K, D, dom.IUpdate, dom.IDelta] =
    this.asInstanceOf[SimpleCell.Aux[K, D, dom.IUpdate, dom.IDelta]]

  def aux1: SimpleCell.Aux1[K, D, Update, Delta, Idx] = this

  def copy(
    idleObservers: List[IdleObserver[Idx]] = idleObservers,
    pendingObservers: List[PendingObserver[Idx]] = pendingObservers,
    blockedIdleObservers: KMap[Token, BlockedIdleObserver[*, Idx]] = blockedIdleObservers,
    blockedPendingObservers: KMap[Token, BlockedPendingObserver[*, Idx]] = blockedPendingObservers,
    nextObserverId: ObserverId = lastObserverId,
    lastToken: Token[_] = lastToken
  ): SimpleCell.Aux1[K, D, Update, Delta, Idx] =
    SimpleCell[K, D, Update, Delta, Idx](value)(idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, nextObserverId, lastToken)

  def hasObserver: Boolean = idleObservers.nonEmpty || pendingObservers.nonEmpty || blockedIdleObservers.nonEmpty || blockedPendingObservers.nonEmpty

  private def hasPendingObservers: Boolean = pendingObservers.nonEmpty

  def update[J](u: Update[J])(implicit dom: IDom.Aux[D, Update, Delta]): CellUpdateResult[SimpleCell.Aux1[K, D, Update, Delta, J]] =
    dom.iUpdate(value, u) match {
      case up @ Updated(newVal, delta) =>
        val pending0: List[PendingObserver[J]] = pendingObservers.map(_.addDelta(delta))
        val pending1: List[PendingObserver[J]] = idleObservers.map(_.addDelta(delta))
        val pending = pending1 ::: pending0

        val blocked0 = blockedPendingObservers.mapValues[BlockedPendingObserver[*, J]](
          new (BlockedPendingObserver[*, Idx] ~> BlockedPendingObserver[*, J]) {
            override def apply[I0](o: BlockedPendingObserver[I0, Idx]) = o.addDelta(delta)
          }
        )
        val blocked1 = blockedIdleObservers.mapValues[BlockedPendingObserver[*, J]](
          new (BlockedIdleObserver[*, Idx] ~> BlockedPendingObserver[*, J]) {
            override def apply[I0](o: BlockedIdleObserver[I0, Idx]) = o.addDelta(delta)
          }
        )
        val blocked = blocked0 ++ blocked1

        val cell = SimpleCell[K, D, Update, Delta, J](newVal)(Nil, pending, KMap[Token, BlockedIdleObserver[*, J]](), blocked, lastObserverId, lastToken)
        val becameDirty = !this.hasPendingObservers && cell.hasPendingObservers
        CellUpdated(cell, becameDirty)

      case Unchanged() => CellUnchanged
    }

  def observe(self: CellIncarnationId, f: PreHandler): (SimpleCell.Aux1[K, D, Update, Delta, Idx], Option[ObserverId], Lst[K[Unit]]) = {
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

  def hold(f: [i] => (D[i], Token[i], ObserverId) => K[Unit]): (SimpleCell.Aux1[K, D, Update, Delta, Idx], ObserverId, Lst[K[Unit]]) = {
    val (cell, token, oid) = block0
    val k = f(value, token, oid)
    (cell, oid, Lst.singleton(k))
  }

  private def block0: (SimpleCell.Aux1[K, D, Update, Delta, Idx], Token[Idx], ObserverId) = {
    val token = lastToken.inc[Idx]
    val oid = lastObserverId.inc
    (
      SimpleCell[K, D, Update, Delta, Idx](value)(
        idleObservers,
        pendingObservers,
        blockedIdleObservers.put(token)(BlockedIdleObserver[D, Delta, Idx](oid)),
        blockedPendingObservers,
        oid,
        token,
      ),
      token,
      oid,
    )
  }

  private def addObserver(f: Handler[Idx]): (SimpleCell.Aux1[K, D, Update, Delta, Idx], ObserverId) = {
    val oid = lastObserverId.inc
    val obs = IdleObserver(oid, f)
    (SimpleCell[K, D, Update, Delta, Idx](value)(obs :: idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, oid, lastToken), oid)
  }

  def rmObserver(oid: ObserverId): (SimpleCell.Aux1[K, D, Update, Delta, Idx], Lst[K[Unit]]) =
    (rmObserver0(oid), Lst.empty)

  def rmObserver0(oid: ObserverId): SimpleCell.Aux1[K, D, Update, Delta, Idx] = {
    listRemoveFirst(idleObservers)(_.id === oid) match {
      case Some(idles) => copy(idleObservers = idles)
      case None => listRemoveFirst(pendingObservers)(_.id === oid) match {
        case Some(pendings) => copy(pendingObservers = pendings)
        case None => mapRemoveFirst(blockedIdleObservers)(_.value.id === oid) match {
          case Some(blockedIdles) => copy(blockedIdleObservers = blockedIdles)
          case None => mapRemoveFirst(blockedPendingObservers)(_.value.id === oid) match {
            case Some(blockedPendings) => copy(blockedPendingObservers = blockedPendings)
            case None => this
          }
        }
      }
    }
  }

  def resume[I0](self: CellIncarnationId, token: Token[I0], trigger: Trigger[I0]): (SimpleCell.Aux1[K, D, Update, Delta, Idx], Lst[K[Unit]], Boolean) = {
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
        val nextToken = lastToken.inc[I0]
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

  private def resumeWithHandler[I0](token: Token[I0], handler: Handler[I0]): (SimpleCell.Aux1[K, D, Update, Delta, Idx], Boolean) =
    blockedIdleObservers.get(token) match {
      case Some(obs) =>
        assert(blockedPendingObservers.get(token).isEmpty)
        val obs1 = obs.resume(handler)
        (SimpleCell[K, D, Update, Delta, Idx](value)(obs1 :: idleObservers, pendingObservers, blockedIdleObservers - token, blockedPendingObservers, lastObserverId, lastToken), false)
      case None => blockedPendingObservers.get(token) match {
        case Some(obs) =>
          val obs1 = obs.resume(handler)
          val becameDirty = pendingObservers.isEmpty
          (SimpleCell[K, D, Update, Delta, Idx](value)(idleObservers, obs1 :: pendingObservers, blockedIdleObservers, blockedPendingObservers - token, lastObserverId, lastToken), becameDirty)
        case None =>
          sys.error(s"unrecognized token $token")
      }
    }

  def triggerPendingObservers(self: CellIncarnationId): (SimpleCell.Aux1[K, D, Update, Delta, Idx], Lst[K[Unit]]) = {
    // @tailrec // can't use because pattern matching sucks, so we use Church encoding (fold)
    def go(
      pending: List[PendingObserver[Idx]],
      idleAcc: List[IdleObserver[Idx]],
      blockedIdleAcc: KMap[Token, BlockedIdleObserver[*, Idx]],
      firedAcc: Lst[K[Unit]],
      lastToken: Token[_]
    ): (SimpleCell.Aux1[K, D, Update, Delta, Idx], Lst[K[Unit]]) =
      pending match {
        case Nil => (SimpleCell[K, D, Update, Delta, Idx](value)(idleAcc, Nil, blockedIdleAcc, blockedPendingObservers, lastObserverId, lastToken), firedAcc)
        case po :: tail =>
          po.handler.handle(value, po.delta).fold(
            caseDiscard = go(tail, idleAcc, blockedIdleAcc, firedAcc, lastToken),
            caseFire = k => go(tail, idleAcc, blockedIdleAcc, k :: firedAcc, lastToken),
            caseSleep = h => go(tail, IdleObserver(po.id, h) :: idleAcc, blockedIdleAcc, firedAcc, lastToken),
            caseFireReload = (k, h) => go(tail, IdleObserver(po.id, h) :: idleAcc, blockedIdleAcc, k :: firedAcc, lastToken),
            caseReconsider = f => {
              val token = lastToken.inc[Idx]
              val k = f((self, token))
              go(tail, idleAcc, blockedIdleAcc.put(token)(BlockedIdleObserver(po.id, Leibniz.refl[Idx])), k :: firedAcc, token)
            }
          )
      }

    go(pendingObservers, idleObservers, blockedIdleObservers, Lst.empty, lastToken)
  }
}

private[nutcracker] object SimpleCell {
  import Cell._

  type AuxD[K[_], D[_], Δ[_, _]] = SimpleCell[K, D] { type Delta[I, J] = Δ[I, J] }
  type Aux[K[_], D[_], U[_], Δ[_, _]] = SimpleCell[K, D] { type Update[J] = U[J]; type Delta[I, J] = Δ[I, J] }
  type Aux1[K[_], D[_], U[_], Δ[_, _], I] = SimpleCell[K, D] { type Update[J] = U[J]; type Delta[I, J] = Δ[I, J]; type Idx = I }

  def init[K[_], D[_], I](d: D[I])(implicit dom: IDom[D]): SimpleCell.Aux[K, D, dom.IUpdate, dom.IDelta] =
    SimpleCell[K, D, dom.IUpdate, dom.IDelta, I](d)(
      Nil,
      Nil,
      KMap(),
      KMap(),
      ObserverId.zero,
      Token.zero
    )

  def apply[K[_], D[_], U[_], Δ[_, _], I](d: D[I])(
    idleObservers0: List[IdleObserver[K, D, Δ, I]],
    pendingObservers0: List[PendingObserver[K, D, Δ, I]] = Nil,
    blockedIdleObservers0: KMap[Token, BlockedIdleObserver[D, Δ, *, I]],
    blockedPendingObservers0: KMap[Token, BlockedPendingObserver[D, Δ, *, I]] = KMap[Token, BlockedPendingObserver[D, Δ, *, I]](),
    lastObsId: ObserverId,
    lastTok: Token[_]
  ): SimpleCell.Aux1[K, D, U, Δ, I] = new SimpleCell[K, D] {
    type Update[J] = U[J]
    type Delta[D1, D2] = Δ[D1, D2]
    type Idx = I

    val value = d
    val idleObservers = idleObservers0
    val pendingObservers = pendingObservers0
    val blockedIdleObservers = blockedIdleObservers0
    val blockedPendingObservers = blockedPendingObservers0
    val lastObserverId = lastObsId
    val lastToken = lastTok
  }
}

private[nutcracker] sealed abstract class OnDemandCell[K[_], D[_]] extends Cell[K, D] {
  val cycle: CellCycle[D]

  def getValue: Option[D[Idx]]

  def observe(self: CellIncarnationId, f: SeqPreHandler[Tok, K, D, Delta]): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]])

  def rmObserver(oid: ObserverId): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def hold(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]])

  def resume[I0](self: CellIncarnationId, token: Token[I0], trigger: Trigger[I0]): (Option[(OnDemandCell[K, D], Boolean)], Lst[K[Unit]])

  def supply[I](self: CellIncarnationId, value: D[I]): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def exclUpdate[J](u: Update[J])(implicit dom: IDom.Aux[D, Update, Delta]): CellUpdateResult[OnDemandCell[K, D]]

  def addFinalizer(sub: Subscription[K]): (Lst[K[Unit]], OnDemandCell[K, D], FinalizerId)

  def removeFinalizer(fid: FinalizerId): OnDemandCell[K, D]

  def triggerPendingObservers(self: CellIncarnationId): (Option[OnDemandCell[K, D]], Lst[K[Unit]])

  def infer(implicit dom: IDom[D]): OnDemandCell.Aux[K, D, dom.IUpdate, dom.IDelta] =
    this.asInstanceOf[OnDemandCell.Aux[K, D, dom.IUpdate, dom.IDelta]]
}

object OnDemandCell {
  type AuxD[K[_], D[_], Δ[_, _]] = OnDemandCell[K, D] { type Delta[I, J] = Δ[I, J] }
  type Aux[K[_], D[_], U[_], Δ[_, _]] = OnDemandCell[K, D] { type Update[J] = U[J]; type Delta[I, J] = Δ[I, J] }
}

private[nutcracker] case class InitializingCell[K[_], D[_], U[_], Δ[_, _]](
  cycle: CellCycle[D],
  preHandlers: List[(ObserverId, SeqPreHandler[λ[α => (Cell.IncarnationId[K, D], Token[α])], K, D, Δ])],
  preHandlersM: List[(ObserverId, [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit])],
  lastObserverId: ObserverId
) extends OnDemandCell[K, D] {
  import Cell._

  type Update[I] = U[I]
  type Delta[I, J] = Δ[I, J]
  // type Value = Nothing

  require(preHandlers.nonEmpty || preHandlersM.nonEmpty)

  private val observerCount = preHandlers.size + preHandlersM.size

  override def getValue: Option[D[Idx]] = None

  override def observe(self: CellIncarnationId, f: PreHandler): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = copy[K, D, U, Δ](preHandlers = (obsId, f) :: preHandlers, lastObserverId = obsId)
    (cell, Some(obsId), Lst.empty)
  }

  override def hold(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]]) = {
    val obsId = lastObserverId.inc
    val cell = copy[K, D, U, Δ](preHandlersM = (obsId, f) :: preHandlersM, lastObserverId = obsId)
    (cell, obsId, Lst.empty)
  }

  override def supply[I](self: CellIncarnationId, value: D[I]): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    var idleObservers: List[IdleObserver[I]] = Nil
    var blockedIdleObservers: KMap[Token, BlockedIdleObserver[*, I]] = KMap()
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
        val tok = token.inc[I]
        ks = cont((self, tok)) :: ks
        blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver[D, Delta, I](id))
        token = tok
      }
    )})

    preHandlersM.foreach({ case (id, f) => {
      val tok = token.inc[I]
      ks = f(value, cycle, tok, id) :: ks
      blockedIdleObservers = blockedIdleObservers.put(tok)(BlockedIdleObserver[D, Delta, I](id))
      token = tok
    }})

    if(idleObservers.nonEmpty || blockedIdleObservers.nonEmpty) {
      val cell = new ActiveCell(cycle, Map(), FinalizerId.zero, lastObserverId, token, value, idleObservers, blockedIdleObservers)
      (Some(cell), ks)
    } else {
      (None, ks)
    }
  }

  override def exclUpdate[J](u: U[J])(implicit dom: IDom.Aux[D, U, Δ]): CellUpdateResult[OnDemandCell[K, D]] = {
    sys.error("Unreachable code: no one has access to the current cycle yet")
  }

  override def addFinalizer(sub: Subscription[K]): (Lst[K[Unit]], OnDemandCell[K, D], FinalizerId) =
    sys.error("Unreachable code: no one has access to the current cycle yet")

  override def removeFinalizer(fid: FinalizerId): OnDemandCell[K, D] =
    sys.error("Unreachable code: no one has access to the current cycle yet")

  override def resume[I0](self: CellIncarnationId, token: Token[I0], trigger: Trigger[I0]): (Option[(OnDemandCell.Aux[K, D, U, Δ], Boolean)], Lst[K[Unit]]) =
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
  def init[K[_], D[_], U[_], Δ[_, _]](cycle: CellCycle[D], f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): (InitializingCell[K, D, U, Δ], ObserverId) = {
    val obsId = ObserverId.zero
    (InitializingCell(cycle, Nil, (obsId, f) :: Nil, obsId), obsId)
  }

  def init[K[_], D[_], U[_], Δ[_, _]](cycle: CellCycle[D], f: SeqPreHandler[λ[α => (Cell.IncarnationId[K, D], Token[α])], K, D, Δ]): (InitializingCell[K, D, U, Δ], ObserverId) = {
    val obsId = ObserverId.zero
    (InitializingCell(cycle, (obsId, f) :: Nil, Nil, obsId), obsId)
  }
}

private[nutcracker] case class ActiveCell[K[_], D[_], U[_], Δ[_, _], I](
  cycle: CellCycle[D],
  impl: SimpleCell.Aux1[K, D, U, Δ, I],
  finalizers: Map[FinalizerId, Subscription[K]],
  lastFinalizerId: FinalizerId
) extends OnDemandCell[K, D] {
  type Update[J] = U[J]
  type Delta[I, J] = Δ[I, J]
  type Idx = I

  require(impl.hasObserver)

  def this(
    cycle: CellCycle[D],
    finalizers: Map[FinalizerId, Subscription[K]],
    lastFinalizerId: FinalizerId,
    lastObserverId: ObserverId,
    lastToken: Token[_],
    value: D[I],
    idleObservers: List[Cell.IdleObserver[K, D, Δ, I]],
    blockedIdleObservers: KMap[Token, Cell.BlockedIdleObserver[D, Δ, *, I]]
  ) = this(
    cycle,
    SimpleCell[K, D, U, Δ, I](value)(idleObservers0 = idleObservers, blockedIdleObservers0 = blockedIdleObservers, lastObsId = lastObserverId, lastTok = lastToken),
    finalizers,
    lastFinalizerId
  )

  override def getValue: Option[D[I]] = Some(impl.value)

  override def observe(self: CellIncarnationId, f: PreHandler): (OnDemandCell[K, D], Option[ObserverId], Lst[K[Unit]]) = {
    val (cell, oid, ks) = impl.observe(self, f)
    (copy(impl = cell), oid, ks)
  }

  override def hold(f: [i] => (D[i], CellCycle[D], Token[i], ObserverId) => K[Unit]): (OnDemandCell[K, D], ObserverId, Lst[K[Unit]]) = {
    val (cell, oid, ks) = impl.hold([i] => (d: D[i], t: Token[i], oid: ObserverId) => f(d, cycle, t, oid))
    (copy(impl = cell), oid, ks)
  }

  override def resume[I0](self: CellIncarnationId, token: Token[I0], trigger: Trigger[I0]): (Option[(OnDemandCell[K, D], Boolean)], Lst[K[Unit]]) = {
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

  override def supply[I](self: CellIncarnationId, value: D[I]): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) =
    sys.error("trying to initialize a cell twice in the same cell cycle")

  override def triggerPendingObservers(self: CellIncarnationId): (Option[OnDemandCell[K, D]], Lst[K[Unit]]) = {
    val (cell, ks) = impl.triggerPendingObservers(self)
    if(cell.hasObserver) (Some(copy(impl = cell)), ks)
    else (None, ks ++ collectFinalizers)
  }

  override def exclUpdate[J](u: U[J])(implicit dom: IDom.Aux[D, U, Δ]): CellUpdateResult[OnDemandCell[K, D]] = {
    impl.update(u) match {
      case CellUpdated(cell, becameDirty) => CellUpdated(ActiveCell[K, D, cell.Update, cell.Delta, J](cycle, cell, finalizers, lastFinalizerId), becameDirty)
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

private[nutcracker] final class Token[A] private(val id: Long) extends AnyVal {
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