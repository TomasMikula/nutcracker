package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.util.typealigned.APair
import nutcracker.util.{FreeK, FreeKT, FunctorKA, HEqualK, Index, InjectK, KMap, KMapB, Lst, ShowK, StateInterpreter, Step, Uncons, WriterState, `Forall{* -> *}`}

import scalaz.Id._
import scalaz.{Equal, Leibniz, Monad, Show, StateT, |>=|, ~>}
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}

import scala.annotation.tailrec
import scalaz.Leibniz.===

private[nutcracker] object PropagationModuleImpl extends Propagation.Module {
  type Ref[A] = DRef[A]
  type Lang[K[_], A] = PropagationLang[Ref, Token, K, A]
  type State[K] = PropagationStore[K]

  implicit val refEquality: HEqualK[Ref] = DRef.equalKInstance
  implicit def refShow: ShowK[Ref] = DRef.showKInstance
  implicit def functorKAPropLang: FunctorKA[Lang] = PropagationLang.functorKInstance[Ref, Token]
  implicit def propagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] =
    PropagationLang.freePropagation[Ref, Token, F]

  def empty[K]: PropagationStore[K] =
    PropagationStore[K](
      nextId = 0L,
      domains = KMap[DRef, Cell[K, ?]](),
      selTriggers = KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => (Option[K], Boolean)]], HList](),
      cellsToSels = Index.empty(sel => sel.cells),
      unresolvedVars = Set(),
      failedVars = Set(),
      dirtyDomains = Set(),
      dirtySelections = Set()
    )

  def emptyF[F[_[_], _]]: PropagationStore[FreeK[F, Unit]] =
    empty[FreeK[F, Unit]]

  def interpreter: StateInterpreter[Lang, State] =
    new StateInterpreter[PropagationLang[Ref, Token, ?[_], ?], PropagationStore] {

      def step: Step[PropagationLang[Ref, Token, ?[_], ?], PropagationStore] =
        new Step[PropagationLang[Ref, Token, ?[_], ?], PropagationStore] {
          import PropagationLang._
          override def apply[K[_], A](p: PropagationLang[Ref, Token, K, A]): WriterState[Lst[K[Unit]], PropagationStore[K[Unit]], A] = WriterState(s =>
            p match {
              case NewCell(d, dom) => s.addVariable(d, dom) match {
                case (s1, ref) => (Lst.empty, s1, ref)
              }
              case Observe(ref, f, dom) => s.addDomainObserver[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom) match {
                case (s1, ko) => (Lst.maybe(ko), s1, ())
              }
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
        new `Forall{* -> *}`[λ[K => StateT[Option, PropagationStore[K], Lst[K]]]] {
          override def compute[K]: StateT[Option, PropagationStore[K], Lst[K]] =
            StateT(_.uncons)
        })
    }

  def dfsSolver: DFSSolver[PropagationLang[Ref, Token, ?[_], ?], PropagationStore, Id, λ[A => Ref[Promise[A]]]] = {
    implicit val mfp: Monad[FreeKT[PropagationLang[Ref, Token, ?[_], ?], Id, ?]] = FreeKT.freeKTMonad[PropagationLang[Ref, Token, ?[_], ?], Id]
    new DFSSolver[PropagationLang[Ref, Token, ?[_], ?], PropagationStore, Id, λ[A => Ref[Promise[A]]]](
      interpreter.freeInstance,
      emptyF[PropagationLang[Ref, Token, ?[_], ?]],
      naiveAssess[FreeK[PropagationLang[Ref, Token, ?[_], ?], ?]],
      fetch
    )
  }

  def naiveAssess[K[_], S[_]](
    lens: Lens[S[K[Unit]], State[K[Unit]]])(implicit
    ord: K |>=| FreeK[Lang, ?]
  ): S[K[Unit]] => Assessment[List[K[Unit]]] =
    s => (naiveAssess[K](ord))(lens.get(s))

  def fetch[K, D](s: State[K])(ref: Ref[D]): D =
    s.fetch(ref)

  def fetchResult[K, D](s: State[K])(ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out] =
    s.fetchResult(ref)

  private val fetch: λ[A => Ref[Promise[A]]] ~> (PropagationStore[FreeK[PropagationLang[Ref, Token, ?[_], ?], Unit]] => ?) =
    λ[λ[A => Ref[Promise[A]]] ~> (PropagationStore[FreeK[PropagationLang[Ref, Token, ?[_], ?], Unit]] => ?)](
      pa => s => s.fetchResult(pa).get
    )

  private def naiveAssess[K[_]](implicit ord: K |>=| FreeK[PropagationLang[Ref, Token, ?[_], ?], ?]): PropagationStore[K[Unit]] => Assessment[List[K[Unit]]] =
    s => s match {
      case ps @ PropagationStore(_, _, _, _, _, _, _, _) => ps.naiveAssess(ord[Unit](_))
    }
}


private[nutcracker] case class PropagationStore[K] private(
  nextId: Long,
  domains: KMap[DRef, Cell[K, ?]],
  selTriggers: KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => (Option[K], Boolean)]], HList],
  cellsToSels: Index[DRef[_], Sel[DRef, _ <: HList]],
  unresolvedVars: Set[DRef[D] forSome { type D }],
  failedVars: Set[Long],
  dirtyDomains: Set[DRef[_]],
  dirtySelections: Set[Sel[DRef, _ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: DRef ~> shapeless.Id = new (DRef ~> shapeless.Id) {
    def apply[D](cell: DRef[D]): D = fetch(cell)
  }

  def addVariable[D](d: D, dom: IDom[D]): (PropagationStore[K], DRef[D]) = {
    val ref = DRef[D](nextId)(dom)
    val domains1 = domains.put(ref)(Cell.init(d, dom))
    val (unresolvedVars1, failedVars1) = dom.assess(d) match {
      case Dom.Failed => (unresolvedVars, failedVars + nextId)
      case Dom.Refined => (unresolvedVars, failedVars)
      case Dom.Unrefined(_) => (unresolvedVars + ref, failedVars)
    }
    (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: DRef[D]): D = domains(ref).value

  def fetchResult[D](ref: DRef[D])(implicit fin: Final[D]): Option[fin.Out] = fin.extract(fetch(ref))

  def fetchVector[D, N <: Nat](refs: Sized[Vector[DRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def update[D, U, Δ[_, _]](ref: DRef[D], u: U)(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] =
    domains(ref).infer.update(u) match {
      case None => this
      case Some(cell) =>
        val (unresolvedVars1, failedVars1) = cell.assess match {
          case Dom.Failed => (unresolvedVars - ref, failedVars + ref.domainId)
          case Dom.Refined => (unresolvedVars - ref, failedVars)
          case Dom.Unrefined(_) => (unresolvedVars, failedVars)
        }
        val domains1 = domains.put(ref.infer)(cell)
        val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
        copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }

  def addDomainObserver[D, U, Δ[_, _]](ref: DRef[D], f: SeqPreHandler[Token, K, D, Δ])(implicit dom: IDom.Aux[D, U, Δ]): (PropagationStore[K], Option[K]) = {
    val (cell, ko) = domains(ref).infer.observe(f)
    val s1 = copy(domains = domains.put(ref)(cell))
    (s1, ko)
  }

  def resume[D, U, Δ[_, _], D0 <: D](ref: DRef[D], token: Token[D0], handler: SeqHandler[Token, K, D, Δ, D0])(implicit dom: IDom.Aux[D, U, Δ]): PropagationStore[K] = {
    val cell = domains(ref).infer.resume(token, handler)
    val dirtyDomains1 = if(cell.hasPendingObservers) dirtyDomains + ref else dirtyDomains
    copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains1)
  }


  def addSelTrigger[L <: HList](sel: Sel[DRef, L], t: L => (Option[K], Boolean)): (PropagationStore[K], Option[K]) = {
    val (ko, keep) = t(sel.fetch(cellFetcher))
    if(keep) (addSelTrigger0(sel, t), ko)
    else     (this,                   ko)
  }

  private def addSelTrigger0[L <: HList](sel: Sel[DRef, L], t: L => (Option[K], Boolean)): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers.put(sel)(t :: selTriggers.getOrElse(sel)(Nil)),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  private def triggersForSel[L <: HList](sel: Sel[DRef, L]): (PropagationStore[K], Lst[K]) = {
    val d = sel.fetch(cellFetcher)
    collectSelTriggers(d, selTriggers.getOrElse(sel)(Nil)) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers.put(sel)(forLater)), fired)
    }
  }

  private def getSelsForCell(ref: DRef[_]): Set[Sel[DRef, _ <: HList]] = cellsToSels.get(ref)

  private def collectSelTriggers[L <: HList](l: L, triggers: List[L => (Option[K], Boolean)]): (List[L => (Option[K], Boolean)], Lst[K]) =
    triggers match {
      case Nil => (Nil, Lst.empty)
      case t :: ts =>
        val (ts1, ks) = collectSelTriggers(l, ts)
        val (ko, keep) = t(l)
        if(keep) (t :: ts1, ko ?+: ks)
        else     (     ts1, ko ?+: ks)
    }

  def uncons: Option[(PropagationStore[K], Lst[K])] =
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

  private[nutcracker] def naiveAssess(inj: FreeK[PropagationLang[DRef, Token, ?[_], ?], Unit] => K): Assessment[List[K]] =
    if(failedVars.nonEmpty) Failed
    else if(unresolvedVars.isEmpty) Done
    else {
      def splitDomain[D](ref: DRef[D]): Option[List[K]] = {
        val cell = domains(ref)
        cell.assess match {
          case Dom.Unrefined(choices) => choices() map { _ map { ui => inj(PropagationLang.updateF[PropagationLang[DRef, Token, ?[_], ?], DRef, Token, cell.dom.Domain, cell.Update, cell.Delta](ref)(ui)(cell.dom, implicitly)) } }
          case _ => sys.error("splitDomain should be called on unresolved variables only.")
        }
      }

      unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }
}

private[nutcracker] sealed abstract class Cell[K, D] {
  type Update
  type Delta[_, _]
  type Value <: D

  type Handler[D1] = SeqHandler[Token, K, D, Delta, D1]

  val value: Value
  val dom: IDom.Aux[D, Update, Delta]
  val idleObservers: List[Handler[Value]]
  val pendingObservers: List[APair[Delta[?, Value], Handler]]
  val blockedIdleObservers: KMap[Token, Value === ?]
  val blockedPendingObservers: KMap[Token, Delta[?, Value]]
  val nextTokenId: Long

  def infer(implicit dom: IDom[D]): Cell.Aux[K, D, dom.Update, dom.IDelta] =
    this.asInstanceOf[Cell.Aux[K, D, dom.Update, dom.IDelta]]

  def hasPendingObservers: Boolean = pendingObservers.nonEmpty

  def update(u: Update): Option[Cell.Aux[K, D, Update, Delta]] =
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

        Some(Cell[K, D, up.NewValue](newVal, dom)(Nil, pending, KMap[Token, up.NewValue === ?](), blocked, nextTokenId))

      case Unchanged() => None
    }

  def observe(f: SeqPreHandler[Token, K, D, Delta]): (Cell[K, D], Option[K]) = {
    import SeqTrigger._
    f.handle(value) match {
      case Discard() => (this, None)
      case Fire(k)   => (this, Some(k))
      case Sleep(h)  => (addObserver(h), None)
      case FireReload(cont) =>
        val token = new Token[Value](nextTokenId)
        val k = cont(token)
        (Cell(value, dom)(idleObservers, pendingObservers, blockedIdleObservers.put(token)(Leibniz.refl[Value]), blockedPendingObservers, nextTokenId + 1), Some(k))
    }
  }

  private def addObserver(f: Handler[Value]): Cell.Aux[K, D, Update, Delta] =
    Cell(value, dom)(f :: idleObservers, pendingObservers, blockedIdleObservers, blockedPendingObservers, nextTokenId)

  def resume[D0 <: D](token: Token[D0], handler: Handler[D0]): Cell.Aux[K, D, Update, Delta] =
    blockedIdleObservers.get(token) match {
      case Some(ev) =>
        assert(blockedPendingObservers.get(token).isEmpty)
        val h: Handler[Value] = Leibniz.symm[Nothing, Any, Value, D0](ev).subst[Handler](handler)
        Cell(value, dom)(h :: idleObservers, pendingObservers, blockedIdleObservers - token, blockedPendingObservers, nextTokenId)
      case None => blockedPendingObservers.get(token) match {
        case Some(δ) =>
          val po = APair.of[Delta[?, Value], Handler](δ, handler) // linter:ignore UndesirableTypeInference // otherwise complains about inferred type Any
          Cell(value, dom)(idleObservers, po :: pendingObservers, blockedIdleObservers, blockedPendingObservers - token, nextTokenId)
        case None =>
          sys.error(s"unrecognized token $token")
      }
    }

  def trigger: (Cell[K, D], Lst[K]) = {
    @tailrec def go(
      pending: List[APair[Delta[?, Value], Handler]],
      idleAcc: List[Handler[Value]],
      blockedIdleAcc: KMap[Token, Value === ?],
      firedAcc: Lst[K],
      nextTokenId: Long
    ): (Cell.Aux[K, D, Update, Delta], Lst[K]) =
    pending match {
      case Nil => (Cell[K, D, Value](value, dom)(idleAcc, Nil, blockedIdleAcc, blockedPendingObservers, nextTokenId), firedAcc)
      case dh :: tail =>
        import SeqTrigger._
        val (δ, h) = (dh._1, dh._2)
        (h.handle(value, δ): SeqTrigger[Token, K, D, Delta, Value]) match {
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

  def assess = dom.assess(value)
}

private[nutcracker] object Cell {
  type Aux[K, D, U, Δ[_, _]] = Cell[K, D] { type Update = U; type Delta[D1, D2] = Δ[D1, D2] }

  def init[K, D](d: D, dom: IDom[D]): Cell.Aux[K, D, dom.Update, dom.IDelta] =
    Cell[K, D, D](d, dom)(Nil, Nil, KMap[Token, D === ?](), KMap[Token, dom.IDelta[?, D]](), 0L)

  def apply[K, D, Val <: D](d: Val, idom: IDom[D])(
    idleObservers0: List[SeqHandler[Token, K, D, idom.IDelta, Val]],
    pendingObservers0: List[APair[idom.IDelta[?, Val], SeqHandler[Token, K, D, idom.IDelta, ?]]],
    blockedIdleObservers0: KMap[Token, Val === ?],
    blockedPendingObservers0: KMap[Token, idom.IDelta[?, Val]],
    nextToken: Long
  ): Cell.Aux[K, D, idom.Update, idom.IDelta] = new Cell[K, D] {
    type Update = idom.Update
    type Delta[D1, D2] = idom.IDelta[D1, D2]
    type Value = Val

    val value = d
    val dom = idom
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