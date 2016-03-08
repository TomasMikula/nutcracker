package nutcracker

import scala.language.{existentials, higherKinds}

import monocle.Lens
import nutcracker.Assessment.{Stuck, Incomplete, Done, Failed}
import nutcracker.util.Index
import nutcracker.util.free.{FreeK, StateInterpreter}
import scalaz.Free.Trampoline
import scalaz.StateT
import scalaz.std.option._
import shapeless.{HList, Nat, Sized}

import Domain._

case class PropagationStore[K[_]] private(
  nextId: Long,
  domains: Map[Long, (D, Domain[A, D]) forSome { type A; type D }],
  domainTriggers: Map[CellRef[_], List[_ => Trigger[K]]],
  selTriggers: Map[Sel[_], List[_ => Trigger[K]]],
  cellsToSels: Index[CellRef[_], Sel[_ <: HList]],
  unresolvedVars: Set[DomRef[A, D] forSome { type A; type D }],
  failedVars: Set[Long],
  dirtyDomains: Set[CellRef[D] forSome { type D }],
  dirtySelections: Set[Sel[_ <: HList]]
) {
  import shapeless.PolyDefns.~>

  private val cellFetcher: CellRef ~> shapeless.Id = new ~>[CellRef, shapeless.Id] {
    def apply[D](cell: CellRef[D]): D = fetch(cell)
  }

  def addVariable[A, D](d: D, ev: Domain[A, D]): (PropagationStore[K], DomRef[A, D]) = {
    val domains1 = domains + ((nextId, (d, ev)))
    val ref = DomRef[A, D](nextId)
    val (unresolvedVars1, failedVars1) = ev.values(d) match {
      case Empty() => (unresolvedVars, failedVars + nextId)
      case Just(a) => (unresolvedVars, failedVars)
      case Many(_) => (unresolvedVars + ref, failedVars)
    }
    (copy(nextId = nextId + 1, domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1), ref)
  }

  def fetch[D](ref: CellRef[D]): D = ref match {
    case dr@DomRef(_) => getDomain(dr)._1
  }

  def fetchResult[A, D](ref: DomRef[A, D]): Option[A] = getDomain(ref) match {
    case (d, dom) => dom.values(d) match {
      case Just(a) => Some(a)
      case _ => None
    }
  }

  def fetchVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N]): Sized[Vector[D], N] =
    refs.map(ref => fetch(ref))

  def intersect[D](ref: CellRef[D], d: D): PropagationStore[K] = ref match {
    case dr @ DomRef(_) => intersect(dr, d)
  }

  def intersectVector[D, N <: Nat](refs: Sized[Vector[CellRef[D]], N], values: Sized[Vector[D], N]): PropagationStore[K] =
    (refs zip values).foldLeft[PropagationStore[K]](this)((s, rv) => s.intersect(rv._1, rv._2))

  private def intersect[A, D](ref: DomRef[A, D], d: D): PropagationStore[K] = {
    val (d0, dom) = getDomain(ref)
    dom.refine(d0, d) match {
      case None => this
      case Some(d1) =>
        val (unresolvedVars1, failedVars1) = dom.values(d1) match {
          case Empty() => (unresolvedVars - ref, failedVars + ref.domainId)
          case Just(_) => (unresolvedVars - ref, failedVars)
          case Many(_) => (unresolvedVars, failedVars)
        }
        val domains1 = domains + ((ref.domainId, (d1, dom)))
        val dirtyDomains1 = dirtyDomains + ref
        copy(domains = domains1, unresolvedVars = unresolvedVars1, failedVars = failedVars1, dirtyDomains = dirtyDomains1)
    }
  }

  def addDomainTrigger[D](ref: CellRef[D], t: D => Trigger[K]): (PropagationStore[K], Option[K[Unit]]) = {
    t(fetch(ref)) match {
      case Discard() => (this, None)
      case Sleep() => (addDomainTrigger0(ref, t), None)
      case Fire(cont) => (this, Some(cont))
      case FireReload(cont) => (addDomainTrigger0(ref, t), Some(cont))
    }
  }

  def addSelTrigger[L <: HList](sel: Sel[L], t: L => Trigger[K]): (PropagationStore[K], Option[K[Unit]]) = {
    t(sel.fetch(cellFetcher)) match {
      case Discard() => (this, None)
      case Sleep() => (addSelTrigger0(sel, t), None)
      case Fire(cont) => (this, Some(cont))
      case FireReload(cont) => (addSelTrigger0(sel, t), Some(cont))
    }
  }

  private def addDomainTrigger0[D](ref: CellRef[D], t: D => Trigger[K]): PropagationStore[K] =
    copy(domainTriggers = domainTriggers + ((ref, t :: domainTriggers.getOrElse(ref, Nil))))

  private def addSelTrigger0[L <: HList](sel: Sel[L], t: L => Trigger[K]): PropagationStore[K] = {
    copy(
      selTriggers = selTriggers + ((sel, t :: selTriggers.getOrElse(sel, Nil))),
      cellsToSels = cellsToSels.add(sel)
    )
  }

  def triggersForDomain[D](ref: CellRef[D]): (PropagationStore[K], List[K[Unit]]) = {
    val d = fetch(ref)
    collectTriggers(d, domainTriggers.getOrElse(ref, Nil).asInstanceOf[List[D => Trigger[K]]]) match {
      case (Nil, fired) => (copy(domainTriggers = domainTriggers - ref), fired)
      case (forLater, fired) => (copy(domainTriggers = domainTriggers + ((ref, forLater))), fired)
    }
  }

  def addDomainResolutionTrigger[A, D](ref: DomRef[A, D], f: A => K[Unit]): (PropagationStore[K], Option[K[Unit]]) = {
    val domain = getDomain(ref)._2
    addDomainTrigger(ref, (d: D) => domain.values(d) match {
      case Domain.Empty() => Discard()
      case Domain.Just(a) => Fire(f(a))
      case Domain.Many(_) => Sleep()
    })
  }

  def triggersForSel[L <: HList](sel: Sel[L]): (PropagationStore[K], List[K[Unit]]) = {
    val d = sel.fetch(cellFetcher)
    collectTriggers(d, selTriggers.getOrElse(sel, Nil).asInstanceOf[List[L => Trigger[K]]]) match {
      case (Nil, fired) => (copy(selTriggers = selTriggers - sel, cellsToSels = cellsToSels.remove(sel)), fired)
      case (forLater, fired) => (copy(selTriggers = selTriggers + ((sel, forLater))), fired)
    }
  }

  def getSelsForCell(ref: CellRef[_]): Set[Sel[_ <: HList]] = cellsToSels.get(ref)


  private[nutcracker] def getDomain[A, D](ref: DomRef[A, D]): (D, Domain[A, D]) =
    domains(ref.domainId).asInstanceOf[(D, Domain[A, D])]

  private def setDomain[A, D](ref: DomRef[A, D], d: D, dom: Domain[A, D]): PropagationStore[K] =
    copy(domains = domains + ((ref.domainId, (d, dom))))

  private def collectTriggers[D](d: D, triggers: List[D => Trigger[K]]): (List[D => Trigger[K]], List[K[Unit]]) =
    triggers match {
      case Nil => (Nil, Nil)
      case t :: ts =>
        val (ts1, conts) = collectTriggers(d, ts)
        t(d) match {
          case Discard() => (ts1, conts)
          case Sleep() => (t :: ts1, conts)
          case Fire(cont) => (ts1, cont :: conts)
          case FireReload(cont) => (t :: ts1, cont :: conts)
        }
    }

  private def uncons: Option[(PropagationStore[K], List[K[Unit]])] =
    if(dirtyDomains.nonEmpty) {
      val d = dirtyDomains.head
      val dirtySels = dirtySelections union getSelsForCell(d)
      val (s1, ks) = triggersForDomain(d)
      Some((s1.copy(dirtyDomains = dirtyDomains.tail, dirtySelections = dirtySels), ks))
    } else if(dirtySelections.nonEmpty) {
      val sel = dirtySelections.head
      val (s1, ks) = triggersForSel(sel)
      Some((s1.copy(dirtySelections = dirtySelections.tail), ks))
    }
    else None
}

object PropagationStore {
  import PropagationLang._
  import scalaz.~>

  def empty[K[_]] = PropagationStore[K](
    nextId = 0L,
    domains = Map(),
    domainTriggers = Map(),
    selTriggers = Map(),
    cellsToSels = Index.empty(sel => sel.cells),
    unresolvedVars = Set(),
    failedVars = Set(),
    dirtyDomains = Set(),
    dirtySelections = Set()
  )

  implicit def interpreter: StateInterpreter.Aux[PropagationLang, PropagationStore] =
    new StateInterpreter[PropagationLang] {
      type State[K[_]] = PropagationStore[K]

      def step[K[_]]: PropagationLang[K, ?] ~> λ[A => scalaz.State[State[K], (A, List[K[Unit]])]] =
        new (PropagationLang[K, ?] ~> λ[A => scalaz.State[State[K], (A, List[K[Unit]])]]) {
          override def apply[A](p: PropagationLang[K, A]): scalaz.State[PropagationStore[K], (A, List[K[Unit]])] = scalaz.State(s =>
            p match {
              case Variable(d, dom) => s.addVariable(d, dom) match {
                case (s1, ref) => (s1, (ref, Nil))
              }
              case VarTrigger(ref, f) => s.addDomainTrigger(ref, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
              case SelTrigger(sel, f) => s.addSelTrigger(sel, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
              case Intersect(ref, d) => (s.intersect(ref, d), ((), Nil))
              case IntersectVector(refs, values) => (s.intersectVector(refs, values), ((), Nil))
              case Fetch(ref) => (s, (s.fetch(ref), Nil))
              case FetchVector(refs) => (s, (s.fetchVector(refs), Nil))
              case WhenResolved(ref, f) => s.addDomainResolutionTrigger(ref, f) match {
                case (s1, ok) => (s1, ((), ok.toList))
              }
            }
          )
        }

      def uncons[K[_]]: StateT[Option, PropagationStore[K], List[K[Unit]]] = StateT(_.uncons)
    }

  def naiveAssess[K[_]](implicit tr: FreeK[PropagationLang, ?] ~> K): PropagationStore[K] => Assessment[List[K[Unit]]] = s => {
    if(s.failedVars.nonEmpty) Failed
    else if(s.unresolvedVars.isEmpty) Done
    else {
      def splitDomain[A, D](ref: DomRef[A, D]): Option[List[K[Unit]]] = {
        val (d, domain) = s.getDomain(ref)
        domain.values(d) match {
          case Domain.Many(branchings) =>
            if(branchings.isEmpty) None
            else Some(branchings.head map { d => tr(intersectF(ref)(d)) })
          case _ => sys.error("splitDomain should be called on unresolved variables only.")
        }
      }

      s.unresolvedVars.toStream.map(splitDomain(_)).collectFirst({
        case Some(branches) => Incomplete(branches)
      }).getOrElse(Stuck)
    }
  }

  def naiveAssess[K[_], S[_[_]]](
    lens: Lens[S[K], PropagationStore[K]])(implicit
    tr: FreeK[PropagationLang, ?] ~> K
  ): S[K] => Assessment[List[K[Unit]]] =
    s => (naiveAssess[K](tr))(lens.get(s))


  private def fetch: Promised ~> (PropagationStore[FreeK[PropagationLang, ?]] => ?) = new ~>[Promised, PropagationStore[FreeK[PropagationLang, ?]] => ?] {
    def apply[A](pa: Promised[A]): (PropagationStore[FreeK[PropagationLang, ?]] => A) = s => s.fetchResult(pa).get
  }
  def dfsSolver: DFSSolver[PropagationLang, PropagationStore, Trampoline, Promised] =
    new DFSSolver[PropagationLang, PropagationStore, Trampoline, Promised](
      interpreter.get[Trampoline](),
      empty[FreeK[PropagationLang, ?]],
      naiveAssess[FreeK[PropagationLang, ?]],
      fetch)
}