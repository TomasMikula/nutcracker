package nutcracker

import scala.language.higherKinds

import nutcracker.PromiseLang._
import nutcracker.PropagationLang._
import nutcracker.DFSSolver._
import nutcracker.Assessment._
import nutcracker.util.free.Interpreter._
import nutcracker.util.free._

import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz.Id._
import scalaz.{Functor, Monoid, StreamT, ~>, -\/}
import scalaz.syntax.applicative._

class DFSSolver {
  type K[A] = FreeK[Lang, A]

  private val DirtyMonoid: Monoid[Dirty[K]] = implicitly[Monoid[Dirty[K]]]
  private val LangFunctor: Functor[Lang[K, ?]] = CoyonedaK.functorInstance[Lang1, K]

  private def init[A](p: K[Promised[A]]): (Store[K], Promised[A]) = {
    import ProductK._

    val emptyD = PropagationStore.empty[K]
    val emptyB: BranchS[K] = BranchStore.empty[StreamT[Id, ?], K]
    val emptyP = PromiseStore.empty[K]
    val empty = emptyD :*: emptyB :*: emptyP

    interpreter.runFree(empty, p)(DirtyMonoid, LangFunctor)
  }

  def solutions[A](p: K[Promised[A]]): StreamT[Id, A] = {
    val (s, pr) = init(p)
    solutions(s, pr)
  }

  private def solutions[A](s: Store[K], pr: Promised[A]): StreamT[Id, A] =
    solutions[A, A](s, pr, StreamT.empty, _ :: StreamT.empty[Id, A])

  def solutions1[A](p: K[Promised[A]]): StreamT[Id, (Option[A], Int)] = {
    val (s, pr) = init(p)
    solutions1(s, pr)
  }

  def allSolutions1[A](p: K[Promised[A]]): (List[A], Int) = {
    solutions1(p).foldLeft((List[A](), 0)) {
      case ((sols, n), (Some(sol), i)) => (sol :: sols, n+i)
      case ((sols, n), (None, i)) => (sols, n+i)
    }
  }

  private def solutions1[A](ps: Store[K], pr: Promised[A]): StreamT[Id, (Option[A], Int)] = {
    val leafs = solutions[A, Option[A]](ps, pr, None :: StreamT.empty[Id, Option[A]], Some(_) :: StreamT.empty[Id, Option[A]])

    def takeOne(s: StreamT[Id, Option[A]]): Option[((Option[A], Int), StreamT[Id, Option[A]])] = takeOne0(s, 0)

    @tailrec def takeOne0(s: StreamT[Id, Option[A]], acc: Int): Option[((Option[A], Int), StreamT[Id, Option[A]])] = s.uncons match {
      case Some((Some(a), tail)) => Some(((Some(a), acc), tail))
      case Some((None, tail)) => takeOne0(tail, acc+1)
      case None =>
        if(acc > 0) Some(((None, acc), StreamT.empty))
        else None
    }

    StreamT.unfold[(Option[A], Int), StreamT[Id, Option[A]]](leafs){ takeOne(_) }
  }

  private def solutions[A, B](
    s: Store[K],
    pr: Promised[A],
    failed: StreamT[Id, B],
    done: A => StreamT[Id, B]): StreamT[Id, B] =
    hideTrampoline(solutionsT(s, pr, failed, done))

  private def solutionsT[A, B](
    ps: Store[K],
    pr: Promised[A],
    failed: StreamT[Id, B],
    done: A => StreamT[Id, B]): StreamT[Trampoline, B] =
    assessAdvance.apply(ps) match {
      case Failed => failed.trans(trampolineId)
      case Done => done(ps._2._2.apply(pr).get).trans(trampolineId)
      case Incomplete(str) =>
        str.trans(trampolineId) flatMap { case (s1, k) => solutionsT(interpreter.runFreeUnit(s1, k)(DirtyMonoid, LangFunctor), pr, failed, done) }
    }
}

object DFSSolver {

  private implicit val BranchInterpreter: Interpreter[BranchL, BranchS, AlwaysClean] = BranchStore.interpreter[StreamT[Id, ?]]
  private val interpreter: Interpreter[Lang, Store, Dirty] = Interpreter.coyonedaInterpreter[Lang1, Store, Dirty]

  private val assessAdvance: AssessAdvance[Store[FreeK[Lang, ?]], StreamT[Id, ?], FreeK[Lang, ?]] = s => {
    type K[A] = FreeK[Lang, A]
    if(s._1.failedVars.nonEmpty) Failed
    else s._2._1.branches match {
      case b::bs =>
        val s1 = s.update_2(s._2.update_1(new BranchStore[StreamT[Id, ?], FreeK[Lang, ?]](bs)))
        Incomplete(b map { k => (s1, k) })
      case Nil =>
        if(s._1.unresolvedVars.isEmpty) Done
        else {

          def splitDomain[A, D](ref: DomRef[A, D]): StreamT[Id, FreeK[Lang, Unit]] = {
            val (d, domain) = s._1.getDomain(ref)
            domain.values(d) match {
              case Domain.Empty() => StreamT.empty
              case Domain.Just(a) => FreeK.Pure[Lang, Unit](()) :: StreamT.empty[Id, FreeK[Lang, Unit]]
              case Domain.Many(branchings) => StreamT.fromIterable(branchings.head) map { d =>
                val p0: PropagationLang[K, Unit] = Intersect(ref, d)
                val p1: Lang1[K, Unit] = CoproductK(-\/(p0))
                val p2: Lang[K, Unit] = CoyonedaK.Pure(p1)
                FreeK.Suspend(p2)
              }
            }
          }

          Incomplete(splitDomain(s._1.unresolvedVars.head) map { k => (s, k) })
        }
    }
  }

  private def hideTrampoline[A](stream: StreamT[Trampoline, A]): StreamT[Id, A] =
    StreamT.unfold[A, StreamT[Trampoline, A]](stream){ _.uncons.run }

  private object trampolineId extends (Id ~> Trampoline) {
    def apply[A](i: A): Trampoline[A] = i.point[Trampoline]
  }
}
