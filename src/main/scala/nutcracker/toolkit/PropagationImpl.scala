package nutcracker.toolkit

import nutcracker.{IDom, OnDemandPropagation, SeqPreHandler, SeqTrigger, Subscription}
import nutcracker.util.{FreeK, HOrderK, Inject, KMap, Lst, ShowK, StateInterpreter, Step, Uncons, WriterState}
import nutcracker.util.ops._
import scala.language.existentials
import scalaz.syntax.equal._
import scalaz.{-\/, Equal, Lens, Monad, Ordering, Show, \/-}
import shapeless.{Nat, Sized}

private[nutcracker] object PropagationImpl extends PersistentOnDemandPropagationModule with FreeOnDemandPropagationToolkit { self =>
  type VarK[K[_], A] = SimpleCellId[K, A]
  type ValK[K[_], A] = CellId[K, A]
  type Lang[K[_], A] = PropagationLang[K, A]
  type StateK[K[_]] = PropagationStore[K]

  override def varOrderK[K[_]]: HOrderK[VarK[K, ?]] = SimpleCellId.orderKInstance
  override def varShowK[K[_]]: ShowK[VarK[K, ?]] = SimpleCellId.showKInstance
  override def valOrderK[K[_]]: HOrderK[ValK[K, ?]] = CellId.orderKInstance
  override def valShowK[K[_]]: ShowK[ValK[K, ?]] = CellId.showKInstance

  override def prgMonad: Monad[FreeK[Lang, ?]] = implicitly
  override implicit def freePropagation[F[_[_], _]](implicit inj: Inject[Lang[FreeK[F, ?], ?], F[FreeK[F, ?], ?]]): OnDemandPropagation[FreeK[F, ?], VarK[FreeK[F, ?], ?], ValK[FreeK[F, ?], ?]] =
    PropagationLang.freePropagation[F]

  override val propagationApi: OnDemandPropagation[Prg, Var, Val] =
    PropagationLang.freePropagation[Lang]

  override def emptyK[K[_]]: PropagationStore[K] =
    PropagationStore.empty[K]

  override def interpret[A](p: Prg[A], s: PropagationStore[Prg]): (PropagationStore[Prg], A) =
    interpreter(Lens.lensId[State]).freeInstance(_.unwrap).apply(p.unwrap).run(s)

  override def interpreter[K[_], S](implicit lens: Lens[S, StateK[K]]): StateInterpreter[K, Lang[K, ?], S] =
    new StateInterpreter[K, Lang[K, ?], S] {

      def step: Step[K, Lang[K, ?], S] = new Step[K, Lang[K, ?], S] {
        import PropagationLang._

        override def apply[A](p: PropagationLang[K, A]): WriterState[Lst[K[Unit]], S, A] = WriterState(s0 => {
          val s = lens.get(s0)
          p match {
            case Update(ref, u, dom) =>
              (Lst.empty, s0 set s.update[dom.Domain, dom.Update, dom.IDelta](ref, u)(dom), ())
            case Observe(ref, f, dom) =>
              val (ks, s1, oid) = s.observe[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom)
              (ks, s0 set s1, oid)
            case ObserveAuto(ref, f, dom) =>
              val (ks, s1, oid) = s.observe[dom.Domain, dom.Update, dom.IDelta](ref, f)(dom)
              (ks, s0 set s1, oid)
            case NewCell(d, dom) =>
              val (s1, ref) = s.newCell(d)(dom) // linter:ignore UndesirableTypeInference
              (Lst.empty, s0 set s1, ref)

            case Hold(ref, f) =>
              val (s1, oid, ks) = s.hold(ref)(f)
              (ks, s0 set s1, oid)
            case HoldAuto(ref, f) =>
              val (s1, cycle, oid, ks) = s.hold(ref)(f) // linter:ignore UndesirableTypeInference
              (ks, s0 set s1, oid)
            case res @ Resume(ref, token, trigger) =>
              val (s1, ks) = s.resume[res.Domain, res.Delta, res.Arg](ref, token, trigger)
              (ks, s0 set s1, ())
            case res @ ResumeAuto(ref, cycle, token, trigger) =>
              val (s1, ks) = s.resume[res.Domain, res.Delta, res.Arg](ref, cycle, token, trigger)
              (ks, s0 set s1, ())
            case RmObserver(ref, oid) =>
              val (s1, ks) = s.rmObserver(ref, oid)
              (ks, s0 set s1, ())
            case RmAutoObserver(ref, cycle, oid) =>
              val (s1, ks) = s.rmObserver(ref, cycle, oid)
              (ks, s0 set s1, ())

            case ExclUpdate(ref, cycle, u, dom) =>
              (Lst.empty, s0 set s.exclUpdate[dom.Domain, dom.Update, dom.IDelta](ref, cycle, u)(dom), ())
            case Supply(ref, cycle, value) =>
              val (s1, ks) = s.supply(ref)(cycle, value)
              (ks, s0 set s1, ())
            case NewAutoCell(setup, dom) =>
              val (s1, ref) = s.newAutoCell(setup)(dom) // linter:ignore UndesirableTypeInference
              (Lst.empty, s0 set s1, ref)
            case AddFinalizer(ref, cycle, sub) =>
              val (ks, s1, fid) = s.addFinalizer(ref, cycle, sub)
              (ks, s0 set s1, fid)
            case RemoveFinalizer(ref, cycle, fid) =>
              val s1 = s.removeFinalizer(ref, cycle, fid)
              (Lst.empty, s0 set s1, ())
          }
        })
      }

      def uncons: Uncons[K, S] = Uncons[K, StateK[K]](_.uncons).zoomOut[S]
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
  failedVars: Set[SimpleCellId[K, _]],
  dirtyDomains: Set[CellId[K, _]]
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

  def uncons: Option[(PropagationStore[K], Lst[K[Unit]])] =
    if(dirtyDomains.nonEmpty) {
      val ref0 = dirtyDomains.head
      val ref: CellId[K, ref0.Domain] = ref0.aux
      ref match {
        case r @ SimpleCellId(_) =>
          val (cell, ks) = simpleCells(r).triggerPendingObservers(-\/(r))
          Some((copy(simpleCells = simpleCells.put(r)(cell), dirtyDomains = dirtyDomains.tail), ks))
        case r @ AutoCellId(_, _) =>
          val cell0 = autoCells(r)
          val (cell, ks) = cell0.triggerPendingObservers(\/-((r, cell0.cycle)))
          cell match {
            case Some(cell) =>
              Some((copy(autoCells = autoCells.put(r)(cell), dirtyDomains = dirtyDomains.tail), ks))
            case None =>
              Some((copy(autoCells = autoCells - r, dirtyDomains = dirtyDomains.tail), ks))
          }
      }
    }
    else None
}

object PropagationStore {
  def empty[K[_]]: PropagationStore[K] = PropagationStore[K](
    lastCellId = CellIdCounter.zero,
    lastCellCycle = CellCycle.zero[Nothing],
    simpleCells = KMap[SimpleCellId[K, ?], SimpleCell[K, ?]](),
    autoCells = KMap[AutoCellId[K, ?], OnDemandCell[K, ?]](),
    failedVars = Set(),
    dirtyDomains = Set()
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

private[nutcracker] final class CellCycle[A] private(val value: Long) extends AnyVal {
  def inc[B]: CellCycle[B] = new CellCycle[B](value + 1)
}

private[nutcracker] object CellCycle {
  def zero[A]: CellCycle[A] = new CellCycle(0)

  implicit def equalInstance[A]: Equal[CellCycle[A]] = new Equal[CellCycle[A]] {
    def equal(a1: CellCycle[A], a2: CellCycle[A]): Boolean = a1.value == a2.value
  }
}