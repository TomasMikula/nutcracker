package nutcracker

import scala.language.higherKinds
import nutcracker.util.typealigned.{APair, BoundedAPair}
import nutcracker.util.{FreeK, HEqualK, HOrderK, Index, InjectK, KMap, KMapB, Lst, ShowK, StateInterpreter, Step, Uncons, WriterState, `Forall{(* -> *) -> *}`}
import scalaz.{Equal, Leibniz, Show, StateT, ~>}
import scalaz.Id._
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}
import scala.annotation.tailrec
import scalaz.Leibniz.===

private[nutcracker] object PropagationImpl extends PersistentPropagationModule with PropagationBundle { self =>
  type Ref[A] = DRef[A]
  type Lang[K[_], A] = PropagationLang[Ref, Token, K, A]
  type State[K[_]] = PropagationStore[K]

  implicit val refEquality: HEqualK[Ref] = DRef.equalKInstance
  implicit def refOrder: HOrderK[Ref] = DRef.orderKInstance
  implicit def refShow: ShowK[Ref] = DRef.showKInstance
  implicit def freePropagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] =
    PropagationLang.freePropagation[Ref, Token, F]

  def propagationApi: Propagation[Prg, Ref] =
    PropagationLang.freePropagation[Ref, Token, Lang]

  def empty[K[_]]: PropagationStore[K] =
    PropagationStore[K](
      nextId = 0L,
      domains = KMap[DRef, Cell[K, ?]](),
      selTriggers = KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList](),
      cellsToSels = Index.empty(sel => sel.cells),
      failedVars = Set(),
      dirtyDomains = Set(),
      dirtySelections = Set()
    )

  def interpret[A](p: Prg[A], s: PropagationStore[Prg]): (PropagationStore[Prg], A) =
    interpreter.freeInstance.apply(p).run(s)

  val interpreter: StateInterpreter[Lang, State] =
    new StateInterpreter[PropagationLang[Ref, Token, ?[_], ?], PropagationStore] {

      def step: Step[PropagationLang[Ref, Token, ?[_], ?], PropagationStore] =
        new Step[PropagationLang[Ref, Token, ?[_], ?], PropagationStore] {
          import PropagationLang._
          override def apply[K[_], A](p: PropagationLang[Ref, Token, K, A]): WriterState[Lst[K[Unit]], PropagationStore[K], A] = WriterState(s =>
            p match {
              case NewCell(d, dom) => s.addVariable(d)(dom) match {
                case (s1, ref) => (Lst.empty, s1, ref)
              }
              case Observe(ref, f, dom) => s.addDomainObserver[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom) match {
                case (s1, ko) => (Lst.maybe(ko), s1, ())
              }
              case Hold(ref) =>
                val (s1, res) = s.block(ref) // linter:ignore UndesirableTypeInference
                (Lst.empty, s1, res)
              case Resume(ref, token, handler, dom) =>
                (Lst.empty, s.resume[dom.Domain, dom.Update, dom.IDelta, token.Arg](ref, token, handler)(dom), ())
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
  domains: KMap[DRef, Cell[K, ?]],
  selTriggers: KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => (Option[K[Unit]], Boolean)]], HList],
  cellsToSels: Index[DRef[_], Sel[DRef, _ <: HList]],
  failedVars: Set[Long],
  dirtyDomains: Set[DRef[_]],
  dirtySelections: Set[Sel[DRef, _ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: DRef ~> shapeless.Id = new (DRef ~> shapeless.Id) {
    def apply[D](cell: DRef[D]): D = fetch(cell)
  }

  def addVariable[D](d: D)(implicit dom: IDom[D]): (PropagationStore[K], DRef[D]) = {
    val ref = DRef[D](nextId)
    val domains1 = domains.put(ref)(Cell.init(d))
    val failedVars1 = if(dom.isFailed(d)) failedVars + nextId else failedVars
    (copy(nextId = nextId + 1, domains = domains1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: DRef[D]): D = domains(ref).value

  def fetchVector[D, N <: Nat](refs: Sized[Vector[DRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def update[D, U, Δ[_, _]](ref: DRef[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] =
    domains(ref).infer.update(u) match {
      case None => this
      case Some(cell) =>
        val failedVars1 = if(cell.isFailed) failedVars + ref.domainId else failedVars
        val domains1 = domains.put(ref.infer)(cell)
        val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
        copy(domains = domains1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }

  def addDomainObserver[D, U, Δ[_, _]](ref: DRef[D], f: SeqPreHandler[Token, K[Unit], D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Option[K[Unit]]) = {
    val (cell, ko) = domains(ref).infer.observe(f)
    (copy(domains = domains.put(ref)(cell)), ko)
  }

  def block[D](ref: DRef[D]): (PropagationStore[K], BoundedAPair[D, Id, Token]) = {
    val (cell, res) = domains(ref).block
    (copy(domains = domains.put(ref)(cell)), res)
  }

  def resume[D, U, Δ[_, _], D0 <: D](ref: DRef[D], token: Token[D0], handler: SeqHandler[Token, K[Unit], D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] = {
    val cell = domains(ref).infer.resume(token, handler)
    val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
    copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains1)
  }


  def addSelTrigger[L <: HList](sel: Sel[DRef, L], t: L => (Option[K[Unit]], Boolean)): (PropagationStore[K], Option[K[Unit]]) = {
    val (ko, keep) = t(sel.fetch(cellFetcher))
    if(keep) (addSelTrigger0(sel, t), ko)
    else     (this,                   ko)
  }

  private def addSelTrigger0[L <: HList](sel: Sel[DRef, L], t: L => (Option[K[Unit]], Boolean)): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers.put(sel)(t :: selTriggers.getOrElse(sel)(Nil)),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  private def triggersForSel[L <: HList](sel: Sel[DRef, L]): (PropagationStore[K], Lst[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectSelTriggers(d, selTriggers.getOrElse(sel)(Nil)) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers.put(sel)(forLater)), fired)
    }
  }

  private def getSelsForCell(ref: DRef[_]): Set[Sel[DRef, _ <: HList]] = cellsToSels.get(ref)

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
      val ref: DRef.Aux[ref0.Domain, ref0.Update, ref0.Delta] = ref0.aux
      val dirtySels = dirtySelections union getSelsForCell(ref)
      val (cell, ks) = domains(ref).trigger
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

  val value: Value
  val idleObservers: List[Handler[Value]]
  val pendingObservers: List[APair[Delta[?, Value], Handler]]
  val blockedIdleObservers: KMap[Token, Value === ?]
  val blockedPendingObservers: KMap[Token, Delta[?, Value]]
  val nextTokenId: Long

  def infer(implicit dom: IDom[D]): Cell.Aux[K, D, dom.Update, dom.IDelta] =
    this.asInstanceOf[Cell.Aux[K, D, dom.Update, dom.IDelta]]

  def hasPendingObservers: Boolean = pendingObservers.nonEmpty

  def update(u: Update)(implicit dom: IDom.Aux[D, Update, Delta]): Option[Cell.Aux[K, D, Update, Delta]] =
    dom.update(value, u) match {
      case up @ Updated(newVal, delta) =>
        val pending0 = pendingObservers.map(dh => APair[Delta[?, up.NewValue], Handler, dh.A](dom.composeDeltas(delta, dh._1), dh._2))
        val pending1 = idleObservers.map(h => APair[Delta[?, up.NewValue], Handler, Value](delta, h))
        val pending = pending1 ::: pending0

        val blocked0 = blockedPendingObservers.mapValues[Delta[?, up.NewValue]](
          λ[Delta[?, Value] ~> Delta[?, up.NewValue]](d => dom.composeDeltas(delta, d))
        )
        val blocked1 = blockedIdleObservers.mapValues[Delta[?, up.NewValue]](
          λ[(Value === ?) ~> Delta[?, up.NewValue]](ev => ev.subst[Delta[?, up.NewValue]](delta))
        )
        val blocked = blocked0 ++ blocked1

        Some(Cell(newVal)(Nil, pending, KMap[Token, up.NewValue === ?](), blocked, nextTokenId))

      case Unchanged() => None
    }

  def observe(f: SeqPreHandler[Token, K[Unit], D, Delta]): (Cell[K, D], Option[K[Unit]]) = {
    import SeqTrigger._
    f.handle(value) match {
      case Discard() => (this, None)
      case Fire(k)   => (this, Some(k))
      case Sleep(h)  => (addObserver(h), None)
      case FireReload(cont) =>
        val (cell, token) = block0
        (cell, Some(cont(token)))
    }
  }

  def block0: (Cell[K, D], Token[Value]) = {
    val token = new Token[Value](nextTokenId)
    (Cell[K, D, Update, Delta, Value](value)(idleObservers, pendingObservers, blockedIdleObservers.put(token)(Leibniz.refl[Value]), blockedPendingObservers, nextTokenId + 1), token)
  }

  def block: (Cell[K, D], BoundedAPair[D, Id, Token]) = {
    val (cell, token) = block0
    (cell, BoundedAPair[D, Id, Token, Value](value, token))
  }

  private def addObserver(f: Handler[Value]): Cell.Aux[K, D, Update, Delta] =
    Cell[K, D, Update, Delta, Value](value)(f :: idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, nextTokenId)

  def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): Cell.Aux[K, D, Update, Delta] =
    blockedIdleObservers.get(token) match {
      case Some(ev) =>
        assert(blockedPendingObservers.get(token).isEmpty)
        val h: Handler[Value] = ev.flip.subst[Handler](handler)
        Cell[K, D, Update, Delta, Value](value)(h :: idleObservers, pendingObservers, blockedIdleObservers - token, blockedPendingObservers, nextTokenId)
      case None => blockedPendingObservers.get(token) match {
        case Some(δ) =>
          val po = APair.of[Delta[?, Value], Handler](δ, handler) // linter:ignore UndesirableTypeInference // otherwise complains about inferred type Any
          Cell[K, D, Update, Delta, Value](value)(idleObservers, po :: pendingObservers, blockedIdleObservers, blockedPendingObservers - token, nextTokenId)
        case None =>
          sys.error(s"unrecognized token $token")
      }
    }

  def trigger: (Cell[K, D], Lst[K[Unit]]) = {
    @tailrec def go(
      pending: List[APair[Delta[?, Value], Handler]],
      idleAcc: List[Handler[Value]],
      blockedIdleAcc: KMap[Token, Value === ?],
      firedAcc: Lst[K[Unit]],
      nextTokenId: Long
    ): (Cell.Aux[K, D, Update, Delta], Lst[K[Unit]]) =
    pending match {
      case Nil => (Cell[K, D, Update, Delta, Value](value)(idleAcc, Nil, blockedIdleAcc, blockedPendingObservers, nextTokenId), firedAcc)
      case dh :: tail =>
        import SeqTrigger._
        val (δ, h) = (dh._1, dh._2)
        (h.handle(value, δ): SeqTrigger[Token, K[Unit], D, Delta, Value]) match {
          case Discard() => go(tail, idleAcc, blockedIdleAcc, firedAcc, nextTokenId)
          case Fire(k) => go(tail, idleAcc, blockedIdleAcc, k :: firedAcc, nextTokenId)
          case Sleep(h) => go(tail, h :: idleAcc, blockedIdleAcc, firedAcc, nextTokenId)
          case FireReload(f) =>
            val token = new Token[Value](nextTokenId)
            val k = f(token)
            go(tail, idleAcc, blockedIdleAcc.put(token)(Leibniz.refl[Value]), k :: firedAcc, nextTokenId + 1)
        }
    }

    go(pendingObservers, idleObservers, blockedIdleObservers, Lst.empty, nextTokenId)
  }

  def isFailed(implicit dom: IDom.Aux[D, Update, Delta]) = dom.isFailed(value)
}

private[nutcracker] object Cell {
  type Aux[K[_], D, U, Δ[_, _]] = Cell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  def init[K[_], D](d: D)(implicit dom: IDom[D]): Cell.Aux[K, D, dom.Update, dom.IDelta] =
    Cell[K, D, dom.Update, dom.IDelta, D](d)(Nil, Nil, KMap[Token, D === ?](), KMap[Token, dom.IDelta[?, D]](), 0L)

  def apply[K[_], D, U, Δ[_, _], Val <: D](d: Val)(
    idleObservers0: List[SeqHandler[Token, K[Unit], D, Δ, Val]],
    pendingObservers0: List[APair[Δ[?, Val], SeqHandler[Token, K[Unit], D, Δ, ?]]],
    blockedIdleObservers0: KMap[Token, Val === ?],
    blockedPendingObservers0: KMap[Token, Δ[?, Val]],
    nextToken: Long
  ): Cell.Aux[K, D, U, Δ] = new Cell[K, D] {
    type Update = U
    type Delta[D1, D2] = Δ[D1, D2]
    type Value = Val

    val value = d
    val idleObservers = idleObservers0
    val pendingObservers = pendingObservers0
    val blockedIdleObservers = blockedIdleObservers0
    val blockedPendingObservers = blockedPendingObservers0
    val nextTokenId = nextToken
  }
}

private[nutcracker] sealed abstract class DRef[D](val domainId: Long) {
  type Domain = D
  type Update
  type Delta[_, _]

  /** Infer `Update` and `Delta` types. Relies on global uniqueness
    * of `Dom[D]` instances.
    */
  def infer(implicit dom: IDom[D]): DRef.Aux[D, dom.Update, dom.IDelta] =
    this.asInstanceOf[DRef.Aux[D, dom.Update, dom.IDelta]]

  def aux: DRef.Aux[Domain, Update, Delta] = this
}

private[nutcracker] object DRef {
  type Aux[D, U, Δ[_, _]] = DRef[D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  def apply[D](domainId: Long)(implicit dom: IDom[D]): DRef.Aux[D, dom.Update, dom.IDelta] =
    new DRef[D](domainId) {
      type Update = dom.Update
      type Delta[D1, D2] = dom.IDelta[D1, D2]
    }

  implicit def equalInstance[D]: Equal[DRef[D]] = new Equal[DRef[D]] {
    def equal(r1: DRef[D], r2: DRef[D]): Boolean = r1.domainId == r2.domainId
  }

  implicit val equalKInstance: HEqualK[DRef] = new HEqualK[DRef] {
    def hEqual[A, B](f1: DRef[A], f2: DRef[B]): Boolean = f1.domainId == f2.domainId
  }

  implicit val orderKInstance: HOrderK[DRef] = new HOrderK[DRef] {
    override def hOrder[A, B](fa: DRef[A], fb: DRef[B]): Int =
      if(fa.domainId < fb.domainId) -1 else if(fa.domainId == fb.domainId) 0 else 1
  }

  implicit def showInstance[D]: Show[DRef[D]] = new Show[DRef[D]] {
    override def shows(ref: DRef[D]): String = s"ref${ref.domainId}"
  }

  implicit def showKInstance: ShowK[DRef] = new ShowK[DRef] {
    def shows[A](ref: DRef[A]): String = s"ref${ref.domainId}"
  }
}

private[nutcracker] final case class Token[A](val id: Long) extends AnyVal {
  type Arg = A
}

private[nutcracker] object Token {
  type Aux[A] = Token[B] forSome { type B <: A }
}