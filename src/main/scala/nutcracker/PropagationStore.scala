package nutcracker

import scala.language.higherKinds
import monocle.Lens
import nutcracker.Assessment.{Done, Failed, Incomplete, Stuck}
import nutcracker.util.{FreeK, FreeKT, FunctorKA, HEqualK, Index, InjectK, K3Map, KMapB, Lst, ShowK, StateInterpreter, Step, Uncons, WriterState, `Forall{* -> *}`}

import scalaz.Id._
import scalaz.{Equal, Monad, Show, StateT, |>=|, ~>}
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}

sealed abstract class PropagationStore[Ref[_], K] {
  def fetch[D](ref: Ref[D]): D
  def fetchResult[D](ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out]

  protected def addVariable[D](d: D, dom: Dom[D]): (PropagationStore[Ref, K], Ref[D])
  protected def addDomainObserver[D, U, Δ](ref: Ref[D], f: D => (Option[K], Option[(D, Δ) => Trigger[K]]))(implicit dom: Dom.Aux[D, U, Δ]): (PropagationStore[Ref, K], Lst[K])
  protected def addSelTrigger[L <: HList](sel: Sel[Ref, L], t: L => Trigger[K]): (PropagationStore[Ref, K], Option[K])
  protected def update[D, U, Δ](ref: Ref[D], u: U)(implicit dom: Dom.Aux[D, U, Δ]): PropagationStore[Ref, K]
  protected def uncons: Option[(PropagationStore[Ref, K], Lst[K])]
}

object PropagationStore {

  trait Module {
    type Ref[_]
    type Lang[K[_], A]
    type State[K]

    implicit def refEquality: HEqualK[Ref]
    implicit def refShow: ShowK[Ref]
    implicit def functorKAPropLang: FunctorKA[Lang]
    implicit def propagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref]

    def empty[K]: State[K]
    def emptyF[F[_[_], _]]: State[FreeK[F, Unit]]
    def interpreter: StateInterpreter[Lang, State]
    def dfsSolver: DFSSolver[Lang, State, Id, λ[A => Ref[Promise[A]]]]

    def naiveAssess[K[_], S[_]](
      lens: Lens[S[K[Unit]], State[K[Unit]]])(implicit
      ord: K |>=| FreeK[Lang, ?]
    ): S[K[Unit]] => Assessment[List[K[Unit]]]

    def fetch[K, D](s: State[K])(ref: Ref[D]): D
    def fetchResult[K, D](s: State[K])(ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out]
  }

  val module: Module = ModuleImpl


  private object ModuleImpl extends Module {
    type Ref[A] = DRef[A]
    type Lang[K[_], A] = PropagationLang[Ref, K, A]
    type State[K] = PropagationStore[Ref, K]

    implicit val refEquality: HEqualK[Ref] = DRef.equalKInstance
    implicit def refShow: ShowK[Ref] = DRef.showKInstance
    implicit def functorKAPropLang: FunctorKA[Lang] = PropagationLang.functorKInstance[Ref]
    implicit def propagation[F[_[_], _]](implicit inj: InjectK[Lang, F]): Propagation[FreeK[F, ?], Ref] =
      PropagationLang.freePropagation[Ref, F]

    def empty[K]: PropagationStore[Ref, K] =
      PropagationStoreImpl[K](
        nextId = 0L,
        domains = K3Map[DRef.Aux, Cell[K, ?, ?, ?]](),
        selTriggers = KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => Trigger[K]]], HList](),
        cellsToSels = Index.empty(sel => sel.cells),
        unresolvedVars = Set(),
        failedVars = Set(),
        dirtyDomains = Set(),
        dirtySelections = Set()
      )

    def emptyF[F[_[_], _]]: PropagationStore[Ref, FreeK[F, Unit]] =
      empty[FreeK[F, Unit]]

    def interpreter: StateInterpreter[Lang, State] =
      PropagationStore.interpreter[Ref]

    def dfsSolver: DFSSolver[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?], Id, λ[A => Ref[Promise[A]]]] = {
      implicit val mfp: Monad[FreeKT[PropagationLang[Ref, ?[_], ?], Id, ?]] = FreeKT.freeKTMonad[PropagationLang[Ref, ?[_], ?], Id]
      new DFSSolver[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?], Id, λ[A => Ref[Promise[A]]]](
        interpreter.freeInstance,
        emptyF[PropagationLang[Ref, ?[_], ?]],
        PropagationStore.naiveAssess[Ref, FreeK[PropagationLang[Ref, ?[_], ?], ?]],
        fetch
      )
    }

    def naiveAssess[K[_], S[_]](
      lens: Lens[S[K[Unit]], State[K[Unit]]])(implicit
      ord: K |>=| FreeK[Lang, ?]
    ): S[K[Unit]] => Assessment[List[K[Unit]]] =
      s => (PropagationStore.naiveAssess[Ref, K](ord))(lens.get(s))

    def fetch[K, D](s: State[K])(ref: Ref[D]): D =
      s.fetch(ref)

    def fetchResult[K, D](s: State[K])(ref: Ref[D])(implicit fin: Final[D]): Option[fin.Out] =
      s.fetchResult(ref)

    private val fetch: λ[A => Ref[Promise[A]]] ~> (PropagationStore[Ref, FreeK[PropagationLang[Ref, ?[_], ?], Unit]] => ?) =
      λ[λ[A => Ref[Promise[A]]] ~> (PropagationStore[Ref, FreeK[PropagationLang[Ref, ?[_], ?], Unit]] => ?)](
        pa => s => s.fetchResult(pa).get
      )
  }

  def naiveAssess[Ref[_], K[_]](implicit ord: K |>=| FreeK[PropagationLang[Ref, ?[_], ?], ?]): PropagationStore[Ref, K[Unit]] => Assessment[List[K[Unit]]] =
    s => s match {
      case ps @ PropagationStoreImpl(_, _, _, _, _, _, _, _) => ps.naiveAssess(ord[Unit](_))
    }


  def interpreter[Ref[_]]: StateInterpreter[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?]] =
    new StateInterpreter[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?]] {

      def step: Step[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?]] =
        new Step[PropagationLang[Ref, ?[_], ?], PropagationStore[Ref, ?]] {
          import PropagationLang._
          override def apply[K[_], A](p: PropagationLang[Ref, K, A]): WriterState[Lst[K[Unit]], PropagationStore[Ref, K[Unit]], A] = WriterState(s =>
            p match {
              case NewCell(d, dom) => s.addVariable(d, dom) match {
                case (s1, ref) => (Lst.empty, s1, ref)
              }
              case Observe(ref, f, dom) => s.addDomainObserver(ref, f)(dom) match {
                case (s1, ks) => (ks, s1, ())
              }
              case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
                case (s1, ok) => (Lst.maybe(ok), s1, ())
              }
              case Update(ref, u, dom) => (Lst.empty, s.update(ref, u)(dom), ())
            }
          )
        }

      def uncons: Uncons[PropagationStore[Ref, ?]] = Uncons[PropagationStore[Ref, ?]](
        new `Forall{* -> *}`[λ[K => StateT[Option, PropagationStore[Ref, K], Lst[K]]]] {
          override def compute[K]: StateT[Option, PropagationStore[Ref, K], Lst[K]] =
            StateT(_.uncons)
        })
    }

  private case class PropagationStoreImpl[K] private(
    nextId: Long,
    domains: K3Map[DRef.Aux, Cell[K, ?, ?, ?]],
    selTriggers: KMapB[λ[`L <: HList` => Sel[DRef, L]], λ[L => List[L => Trigger[K]]], HList],
    cellsToSels: Index[DRef[_], Sel[DRef, _ <: HList]],
    unresolvedVars: Set[DRef[D] forSome { type D }],
    failedVars: Set[Long],
    dirtyDomains: Set[DRef[_]],
    dirtySelections: Set[Sel[DRef, _ <: HList]]
  ) extends PropagationStore[DRef, K] {
    import shapeless.PolyDefns.~>

    private val cellFetcher: DRef ~> shapeless.Id = new (DRef ~> shapeless.Id) {
      def apply[D](cell: DRef[D]): D = fetch(cell)
    }

    private def getDomain[D](ref: DRef[D]): Cell[K, D, ref.Update, ref.Delta] =
      domains(ref)

    def addVariable[D](d: D, dom: Dom[D]): (PropagationStoreImpl[K], DRef[D]) = {
      val ref = DRef[D](nextId)(dom)
      val domains1 = domains.put(ref)(Cell(d, dom, None, Nil))
      val (unresolvedVars1, failedVars1) = dom.assess(d) match {
        case Dom.Failed => (unresolvedVars, failedVars + nextId)
        case Dom.Refined => (unresolvedVars, failedVars)
        case Dom.Unrefined(_) => (unresolvedVars + ref, failedVars)
      }
      (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
    }

    def fetch[D](ref: DRef[D]): D = domains(ref: DRef.Aux[D, ref.Update, ref.Delta]).value

    def fetchResult[D](ref: DRef[D])(implicit fin: Final[D]): Option[fin.Out] = fin.extract(fetch(ref))

    def fetchVector[D, N <: Nat](refs: Sized[Vector[DRef[D]], N]): Sized[Vector[D], N] =
      refs.map(ref => fetch(ref))

    protected def update[D, U, Δ](ref: DRef[D], u: U)(implicit dom: Dom.Aux[D, U, Δ]): PropagationStoreImpl[K] =
      domains(ref.infer).update(u) match {
        case None => this
        case Some(cell) =>
          val (unresolvedVars1, failedVars1) = cell.assess match {
            case Dom.Failed => (unresolvedVars - ref, failedVars + ref.domainId)
            case Dom.Refined => (unresolvedVars - ref, failedVars)
            case Dom.Unrefined(_) => (unresolvedVars, failedVars)
          }
          val domains1 = domains.put(ref.infer)(cell)
          val dirtyDomains1 = dirtyDomains + ref
          copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
      }

    def addDomainObserver[D, U, Δ](ref: DRef[D], f: D => (Option[K], Option[(D, Δ) => Trigger[K]]))(implicit dom: Dom.Aux[D, U, Δ]): (PropagationStoreImpl[K], Lst[K]) = {
      val (now, onChange) = f(fetch(ref))
      onChange match {
        case Some(action) =>
          val (s1, ks) = addDomainObserver0(ref.infer, action)
          (s1, now ?+: ks)
        case None => (this, Lst.maybe(now))
      }
    }

    def addSelTrigger[L <: HList](sel: Sel[DRef, L], t: L => Trigger[K]): (PropagationStoreImpl[K], Option[K]) = {
      t(sel.fetch(cellFetcher)) match {
        case Discard() => (this, None)
        case Sleep() => (addSelTrigger0(sel, t), None)
        case Fire(cont) => (this, Some(cont))
        case FireReload(cont) => (addSelTrigger0(sel, t), Some(cont))
      }
    }

    private def addDomainObserver0[D, U, Δ](ref: DRef.Aux[D, U, Δ], f: (D, Δ) => Trigger[K]): (PropagationStoreImpl[K], Lst[K]) = {
      val (cell, fired) = domains(ref).addObserver(f)
      (copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains - ref), fired)
    }

    private def addSelTrigger0[L <: HList](sel: Sel[DRef, L], t: L => Trigger[K]): PropagationStoreImpl[K] = {
      copy(
        selTriggers = selTriggers.put(sel)(t :: selTriggers.getOrElse(sel)(Nil)),
        cellsToSels = cellsToSels.add(sel)
      )
    }

    private def triggersForSel[L <: HList](sel: Sel[DRef, L]): (PropagationStoreImpl[K], Lst[K]) = {
      val d = sel.fetch(cellFetcher)
      collectSelTriggers(d, selTriggers.getOrElse(sel)(Nil)) match {
        case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
        case (forLater, fired) => (copy(selTriggers = selTriggers.put(sel)(forLater)), fired)
      }
    }

    private def getSelsForCell(ref: DRef[_]): Set[Sel[DRef, _ <: HList]] = cellsToSels.get(ref)

    private def collectSelTriggers[L <: HList](l: L, triggers: List[L => Trigger[K]]): (List[L => Trigger[K]], Lst[K]) =
      triggers match {
        case Nil => (Nil, Lst.empty)
        case t :: ts =>
          val (ts1, conts) = collectSelTriggers(l, ts)
          t(l) match {
            case Discard() => (ts1, conts)
            case Sleep() => (t :: ts1, conts)
            case Fire(cont) => (ts1, cont :: conts)
            case FireReload(cont) => (t :: ts1, cont :: conts)
          }
      }

    protected def uncons: Option[(PropagationStoreImpl[K], Lst[K])] =
      if(dirtyDomains.nonEmpty) {
        val ref0 = dirtyDomains.head
        val ref: DRef.Aux[ref0.Domain, ref0.Update, ref0.Delta] = ref0.aux
        val dirtySels = dirtySelections union getSelsForCell(ref)
        val (cell, ks) = getDomain(ref).trigger
        Some((copy(domains = domains.put(ref)(cell), dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
      } else if(dirtySelections.nonEmpty) {
        val sel = dirtySelections.head
        val (s1, ks) = triggersForSel(sel)
        Some((s1.copy(dirtySelections = dirtySelections.tail), ks))
      }
      else None

    private[nutcracker] def naiveAssess(inj: FreeK[PropagationLang[DRef, ?[_], ?], Unit] => K): Assessment[List[K]] =
      if(failedVars.nonEmpty) Failed
      else if(unresolvedVars.isEmpty) Done
      else {
        def splitDomain[D](ref: DRef[D]): Option[List[K]] = {
          val cell = getDomain(ref)
          cell.assess match {
            case Dom.Unrefined(choices) => choices() map { _ map { ui => inj(Propagation[FreeK[PropagationLang[DRef, ?[_], ?], ?], DRef].update(ref)(cell.dom).by(ui)) } }
            case _ => sys.error("splitDomain should be called on unresolved variables only.")
          }
        }

        unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
          case Some(branches) => Incomplete(branches)
        }).getOrElse(Stuck)
      }
  }

  private[PropagationStore] final case class Cell[K, D, U, Δ](
    value: D,
    dom: Dom.Aux[D, U, Δ],
    delta: Option[Δ],
    observers: List[(D, Δ) => Trigger[K]]
  ) {

    def update(u: U): Option[Cell[K, D, U, Δ]] =
      dom.update(value, u) match {
        case None => None
        case Some((d, δ)) =>
          val delta = this.delta match {
            case Some(δ0) => dom.appendDeltas(δ0, δ)
            case None => δ
          }
          Some(copy(value = d, delta = Some(delta)))
      }

    def addObserver(f: (D, Δ) => Trigger[K]): (Cell[K, D, U, Δ], Lst[K]) = {
      val (remaining, fired) = delta match {
        case Some(δ) => collectFired(value, δ, observers)
        case None => (observers, Lst.empty)
      }
      (copy(delta = None, observers = f :: remaining), fired)
    }

    def trigger: (Cell[K, D, U, Δ], Lst[K]) = delta match {
      case Some(δ) =>
        val (forLater, fired) = collectFired(value, δ, observers)
        (copy(delta = None, observers = forLater), fired)
      case None =>
        (this, Lst.empty)
    }

    def assess = dom.assess(value)

    private def collectFired(d: D, δ: Δ, triggers: List[(D, Δ) => Trigger[K]]): (List[(D, Δ) => Trigger[K]], Lst[K]) =
      triggers match {
        case Nil => (Nil, Lst.empty)
        case t :: ts =>
          val (ts1, conts) = collectFired(d, δ, ts)
          t(d, δ) match {
            case Discard() => (ts1, conts)
            case Sleep() => (t :: ts1, conts)
            case Fire(cont) => (ts1, cont :: conts)
            case FireReload(cont) => (t :: ts1, cont :: conts)
          }
      }
  }

  private[PropagationStore] sealed abstract class DRef[D](private[nutcracker] val domainId: Long) {
    type Domain = D
    type Update
    type Delta

    /** Infer `Update` and `Delta` types. Relies on global uniqueness
      * of `Dom[D]` instances.
      */
    def infer(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
      this.asInstanceOf[DRef.Aux[D, dom.Update, dom.Delta]]

    def aux: DRef.Aux[Domain, Update, Delta] = this
  }

  private object DRef {
    type Aux[D, U, Δ] = DRef[D] { type Update = U; type Delta = Δ }

    private[nutcracker] def apply[D](domainId: Long)(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
      new DRef[D](domainId) {
        type Update = dom.Update
        type Delta = dom.Delta
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
}