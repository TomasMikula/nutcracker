package nutcracker

import nutcracker.ProblemDescription.Bind
import nutcracker.ProblemDescription.Zip
import shapeless.:+:

import scala.language.higherKinds
import scala.annotation.tailrec

import ProblemDescription._

import algebra.lattice.MeetSemilattice
import scalaz.ReaderWriterState
import scalaz.Id.Id
import scalaz.{\/, -\/, \/-, Traverse, StreamT}
import scalaz.std.vector._
import scalaz.syntax.monoid._
import shapeless.{ :: => _, _ }
import shapeless.ops.hlist.{ Zip => _, _ }

case class PartialSolution private(
    domains: Domains,
    promises: Promises,
    branchings: List[Branch[Unit]],
    relations: RelTable,
    triggers: Triggers) {

  import PartialSolution._

  def status: Status =
    if(domains.failedVars.nonEmpty) Failed
    else if(branchings.isEmpty && domains.unresolvedVars.isEmpty) Done
    else Incomplete(((0 until branchings.length) map { BranchingId(_) }).toList, domains.unresolvedVars.toList)

  def branchBy(brId: BranchingId): StreamT[Id, PartialSolution] = {
    val (br1, br2) = branchings.splitAt(brId.id)
    val branching = br2.head
    val ps1 = this.copy(branchings = br1 ++ br2.tail)
    StreamT.fromIterable(branching.branches()).map { interpret(ps1, _) }
  }

  def splitDomain[A, D](ref: PureDomRef[A, D]): StreamT[Id, PartialSolution] = {
    val (d, domain) = domains.getDomain(ref)
    domain.values(d) match {
      case Domain.Empty() => StreamT.empty
      case Domain.Just(a) => this :: StreamT.empty[Id, PartialSolution]
      case Domain.Many(branchings) => StreamT.fromIterable(branchings.head) map { d => interpret(this, Intersect(ref, d)) }
    }
  }

  def getPromised[A](pr: PromiseId[A]): A = promises(pr).get
}

object PartialSolution {
  sealed trait CellRef[D]
  case class PureDomRef[+A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D]

  sealed trait Status
  case object Failed extends Status
  case object Done extends Status
  case class Incomplete(branchings: List[BranchingId], unresolvedDomains: List[PureDomRef[_, _]]) extends Status

  final case class PromiseId[A](val id: Long) extends AnyVal
  final case class BranchingId(val id: Int) extends AnyVal

  private def empty: PartialSolution = new PartialSolution(
      domains = Domains.empty,
      promises = Promises.empty,
      branchings = List(),
      relations = RelTable.empty,
      triggers = Triggers.empty)

  type IStop[A] = A :+: Branch[A] :+: PromiseId[A] :+: CNil

  type WriterState[W, S, A] = ReaderWriterState[Unit, W, S, A]
  object WriterState {
    def apply[W, S, A](f: S => (W, A, S)): WriterState[W, S, A] = ReaderWriterState((r, s) => f(s))
  }

  type InterpreterStep[A] = WriterState[DirtyThings, PartialSolution, A]
  implicit def interpreterStepMonad: scalaz.Monad[InterpreterStep] =
    scalaz.IndexedReaderWriterState.rwstMonad[Id, Unit, DirtyThings, PartialSolution]
  object InterpreterStep {

    def apply[A](f: PartialSolution => (DirtyThings, A, PartialSolution)): InterpreterStep[A] =
      WriterState[DirtyThings, PartialSolution, A](f)

    def pure[A](a: A): InterpreterStep[A] = apply[A](ps => (DirtyThings.empty, a, ps))
    def stopAtPure[A](a: A): InterpreterStep[IStop[A]] = apply[IStop[A]](ps => (DirtyThings.empty, Inl(a), ps))
    def stopAtBranch[A](br: Branch[A]): InterpreterStep[IStop[A]] = apply[IStop[A]](ps => (DirtyThings.empty, Inr(Inl(br)), ps))
    def stopAtPromise[A](pr: PromiseId[A]): InterpreterStep[IStop[A]] = apply[IStop[A]](ps => (DirtyThings.empty, Inr(Inr(Inl(pr))), ps))

    def mapState(f: PartialSolution => PartialSolution): InterpreterStep[Unit] =
      apply[Unit](ps => (DirtyThings.empty, (), f(ps)))

    def wrapStateMonad[A](f: PartialSolution => (PartialSolution, A)): InterpreterStep[A] =
      apply[A](ps => f(ps) match { case (ps1, a) => (DirtyThings.empty, a, ps1) })
  }

  def init[A](pd: ProblemDescription[A]): (PartialSolution, PromiseId[A]) = {
    val (ps, prA) = promise0[A](empty)
    val pd1 = pd flatMap { a => Complete(prA, a) }
    (interpret(ps, pd1), prA)
  }

  def interpret(ps: PartialSolution, p: ProblemDescription[Unit]): PartialSolution =
    interpret(ps, DirtyThings.continuation(p))

  @tailrec
  def interpret(ps: PartialSolution, dirty: DirtyThings): PartialSolution =
    dirty.split match {
      case None => ps
      case Some((d, ds)) => d match {
        case PendingContinuation(cont) => interpret0(cont).run((), ps) match {
          case (dirty1, istop, ps1) =>
            val ps2 = incorporate0(ps1, istop)
            interpret(ps2, ds |+| dirty1)
        }
        case DirtyDomain(ref) =>
          val domainTriggers = ps.triggers.getForDomain(ref)
          ps.domains.getDomain(ref) match {
            case (dom, domain) =>
              val (ps1, domainResolutionTriggers) = domain.values(dom) match {
                case Domain.Empty() => (ps, Nil)
                case Domain.Just(a) => ps.triggers.domainResolvedTriggers(ref, a) match {
                  case (triggers1, conts) => (ps.copy(triggers = triggers1), conts)
                }
                case Domain.Many(_) => (ps, Nil)
              }
              interpret(ps1, ds |+| DirtyThings.continuations(domainTriggers ++ domainResolutionTriggers))
          }
      }
    }

  def interpret0[A](p: ProblemDescription[A]): InterpreterStep[IStop[A]] = p match {
    case Pure(a) => InterpreterStep.stopAtPure(a)
    case Second(p1, p2) => for {
      r1 <- interpret0(p1)
       _ <- incorporate(r1)
      r2 <- interpret0(p2)
    } yield r2
    case br @ Branch(_) => InterpreterStep.stopAtBranch(br)
    case Bind(p, f) => interpret0(p) flatMap {
      case Inl(a) => interpret0(f(a))
      case Inr(Inl(br)) => interpret0(Branch(() => br.branches().map(_.flatMap(f))))
      case Inr(Inr(Inl(pr))) => interpret0(whenComplete(pr)(f))
      case Inr(Inr(Inr(cnil))) => throw new AssertionError("CNil has no values")
    }
    case z @ Zip(pa, pb) => interpret0(pa) flatMap { ia => interpret0(pb) flatMap { ib => (ia, ib) match {
      case (Inl(a), Inl(b)) => InterpreterStep.stopAtPure((a, b))
      case (Inr(Inl(bra)), Inl(b)) => InterpreterStep.stopAtBranch(bra map1 { (_, b) })
      case (Inl(a), Inr(Inl(brb))) => InterpreterStep.stopAtBranch(brb map1 { (a, _) })
      case (Inr(Inl(bra)), Inr(Inr(Inl(prb)))) =>
        interpret0(bra flatMap1 { a => whenComplete(prb)(b => Pure[A]((a, b))) })
      case (Inr(Inr(Inl(pra))), Inr(Inl(brb))) =>
        interpret0(brb flatMap1 { b => whenComplete(pra)(a => Pure[A]((a, b))) })
      case (Inl(a), Inr(Inr(Inl(prb)))) =>
        interpret0(whenComplete(prb)(b => Pure[A]((a, b))))
      case (Inr(Inr(Inl(pra))), Inl(b)) =>
        interpret0(whenComplete(pra)(a => Pure[A]((a, b))))
      case (Inr(Inr(Inl(pra))), Inr(Inr(Inl(prb)))) =>
        interpret0(whenComplete(pra)(a => whenComplete(prb)(b => Pure[A]((a, b)))))
      case (Inr(Inl(bra)), Inr(Inl(brb))) => for {
        pra <- promise[z.Fst]
        _ <- addBranching(bra flatMap1 { a => Complete(pra, a) })
      } yield Inr(Inl(brb flatMap1 { b => whenComplete(pra)(a => Pure[A]((a, b))) }))
      case (Inr(Inr(Inr(cnil))), _) => throw new AssertionError("CNil has no values")
      case (_, Inr(Inr(Inr(cnil)))) => throw new AssertionError("CNil has no values")
    }}}
    case Complete(pid, v) => complete(pid, v) map { Inl(_) }
    case WhenComplete(pr, f) => onComplete(pr)(f) flatMap {
      case -\/(pdA) => interpret0(pdA)
      case \/-(prA) => InterpreterStep.stopAtPromise(prA)
    }
    case WhenResolved(ref, f) => onResolved(ref)(f) flatMap {
      case -\/(pdA) => interpret0(pdA)
      case \/-(prA) => InterpreterStep.stopAtPromise(prA)
    }
    case Variable(d, ev) => addVariable(d, ev) map { Inl(_) }
    case Fetch(ref) => fetch(ref) map { Inl(_) }
    case FetchVector(refs) => fetchVector(refs) map { Inl(_) }
    case Intersect(ref, d) => intersect(ref, d) map { Inl(_) }
    case IntersectVector(refs, values) => intersectVector(refs, values) map { Inl(_) }
    case Relation(rel, refs, toRefs) => relation(rel, refs, toRefs) map { Inl(_) }
    case RelTrigger(rel, f, toRefs) => relTrigger(rel, f, toRefs) map { Inl(_) }
    case VarTrigger(ref, f) => varTrigger(ref, f) map { Inl(_) }
  }

  private def incorporate(istop: IStop[Unit]): InterpreterStep[Unit] =
    InterpreterStep.mapState { incorporate0(_, istop) }

  private def incorporate0(ps: PartialSolution, istop: IStop[Unit]): PartialSolution = istop match {
    case Inl(_) => ps
    case Inr(Inl(br)) => addBranching0(ps, br)
    case Inr(Inr(Inl(_))) => ps // TODO warning about ignored PromiseId
    case Inr(Inr(Inr(cnil))) => throw new AssertionError("CNil has no values")
  }

  private def promise[A]: InterpreterStep[PromiseId[A]] = InterpreterStep.wrapStateMonad(promise0[A])
  private def promise0[A](ps: PartialSolution): (PartialSolution, PromiseId[A]) = ps.promises.promise[A] match {
    case (promises1, prA) => (ps.copy(promises = promises1), prA)
  }

  private def onComplete[A, B](prA: PromiseId[A])(f: A => ProblemDescription[B]): InterpreterStep[ProblemDescription[B] \/ PromiseId[B]] =
    InterpreterStep.wrapStateMonad { ps => ps.promises(prA) match {
      case Some(a) => (ps, -\/(f(a)))
      case None =>
        val (ps1, prB) = promise0[B](ps)
        (ps1.copy(triggers = ps1.triggers.addOnComplete[A](prA, { f(_) flatMap { b => Complete(prB, b) } })), \/-(prB))
    }}

  private def onResolved[A, D, B](ref: PureDomRef[A, D])(f: A => ProblemDescription[B]): InterpreterStep[ProblemDescription[B] \/ PromiseId[B]] =
    InterpreterStep.wrapStateMonad { ps => ps.domains.getDomain(ref) match {
      case (d, domain) => domain.values(d) match {
        case Domain.Empty() => (ps, -\/(ProblemDescription.empty[B]))
        case Domain.Just(a) => (ps, -\/(f(a)))
        case Domain.Many(_) =>
          val (ps1, prB) = promise0[B](ps)
          (ps1.copy(triggers = ps.triggers.addDomainResolutionTrigger[A, D](ref, { f(_) flatMap { b => Complete(prB, b) }})), \/-(prB))
      }
    }}

  private def addBranching(br: Branch[Unit]): InterpreterStep[Unit] =
    InterpreterStep mapState { ps => addBranching0(ps, br) }

  private def addBranching0(ps: PartialSolution, br: Branch[Unit]): PartialSolution = ps.copy(branchings = br :: ps.branchings)

  private def complete[A](pid: PromiseId[A], a: A): InterpreterStep[Unit] = InterpreterStep[Unit] { ps =>
    val promises1 = ps.promises.complete(pid, a)
    val (triggers1, conts) = ps.triggers.promiseCompleted(pid, a)
    (DirtyThings.continuations(conts), (), ps.copy(promises = promises1, triggers = triggers1))
  }
  private def addVariable[A, D](d: D, ev: Domain[A, D]): InterpreterStep[PureDomRef[A, D]] =
    InterpreterStep.wrapStateMonad { ps =>
      ps.domains.addVariable(d, ev) match { case (doms, ref) => (ps.copy(domains = doms), ref) }
    }
  private def varTrigger[A, D](ref: PureDomRef[A, D], f: D => ProblemDescription[Unit]): InterpreterStep[Unit] = {
    val f1: ProblemDescription[Unit] = Fetch(ref).flatMap(f)
    InterpreterStep[Unit] { ps =>
      val triggers1 = ps.triggers.addDomainTrigger(ref, f1)
      (DirtyThings.continuation(f1), (), ps.copy(triggers = triggers1))
    }
  }
  private def fetch[A, D](ref: PureDomRef[A, D]): InterpreterStep[D] =
    InterpreterStep.wrapStateMonad { ps => (ps, ps.domains.fetch(ref)) }
  private def fetchVector[A, D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N]): InterpreterStep[Sized[Vector[D], N]] =
    InterpreterStep.wrapStateMonad { ps => (ps, ps.domains.fetchVector(refs)) }
  private def intersect[A, D](ref: PureDomRef[A, D], d: D): InterpreterStep[Unit] =
    InterpreterStep[Unit] { ps =>
      ps.domains.intersect(ref, d) match {
        case None => (DirtyThings.empty, (), ps)
        case Some(domains1) => (DirtyThings.dirtyDomain(ref), (), ps.copy(domains = domains1))
        // TODO also add dirty constraints when constraints are first class
      }
    }
  private def intersectVector[A, D, N <: Nat](refs: Sized[Vector[PureDomRef[A, D]], N], values: Sized[Vector[D], N]): InterpreterStep[Unit] =
    Traverse[Vector].sequenceU(refs zip values map { case (ref, d) => intersect(ref, d) }) map { vectorOfUnit => () }

  private def relation[L <: HList, Refs <: HList](rel: Rel[L], refs: Refs, toRefs: Mapped.Aux[L, CellRef, Refs]): InterpreterStep[Unit] = ???
  private def relTrigger[L <: HList, Refs <: HList](rel: Rel[L], f: Refs => ProblemDescription[Unit], toRefs: Mapped.Aux[L, CellRef, Refs]): InterpreterStep[Unit] = ???
}