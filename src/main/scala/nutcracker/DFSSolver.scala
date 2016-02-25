package nutcracker

import scala.language.higherKinds
import nutcracker.Assessment._

import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz.Id._
import scalaz.{Monoid, StreamT, ~>}
import scalaz.syntax.applicative._

class DFSSolver[C: Monoid] extends Solver[PropRelCost[C], List] {

  val lang: PropRelCost[C] = new PropRelCost[C]

  def solutions[A](p: K[Promised[A]]): StreamT[Id, A] = {
    val (s, pr) = init(p)
    solutions(s, pr)
  }

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

  def assess(s: S): Assessment[List[(S, K[Unit])]] = lang.naiveAssess(s)

  private def init[A](p: K[Promised[A]]): (S, Promised[A]) = {
    lang.interpreter.runFree(p)
  }

  private def solutions[A](s: S, pr: Promised[A]): StreamT[Id, A] =
    solutions[A, A](s, pr, StreamT.empty, _ :: StreamT.empty[Id, A])

  private def solutions1[A](ps: S, pr: Promised[A]): StreamT[Id, (Option[A], Int)] = {
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
    s: S,
    pr: Promised[A],
    failed: StreamT[Id, B],
    done: A => StreamT[Id, B]): StreamT[Id, B] =
    hideTrampoline(solutionsT(s, pr, failed, done))

  private def solutionsT[A, B](
    s: S,
    pr: Promised[A],
    failed: StreamT[Id, B],
    done: A => StreamT[Id, B]): StreamT[Trampoline, B] =
    assess(s) match {
      case Failed => failed.trans(trampolineId)
      case Stuck => failed.trans(trampolineId) // TODO: Don't treat as failed.
      case Done => done(lang.propStore.get(s).fetchResult(pr).get).trans(trampolineId)
      case Incomplete(str) =>
        StreamT.fromIterable(str).trans(trampolineId) flatMap { case (s1, k) => solutionsT(lang.interpreter.runFreeUnit(s1, k), pr, failed, done) }
    }

  private def hideTrampoline[A](stream: StreamT[Trampoline, A]): StreamT[Id, A] =
    StreamT.unfold[A, StreamT[Trampoline, A]](stream){ _.uncons.run }

  private object trampolineId extends (Id ~> Trampoline) {
    def apply[A](i: A): Trampoline[A] = i.point[Trampoline]
  }
}

object DFSSolver {
  private implicit def unitMonoid: Monoid[Unit] = new Monoid[Unit] {
    def zero: Unit = ()
    def append(f1: Unit, f2: => Unit): Unit = ()
  }

  def apply(): DFSSolver[Unit] = new DFSSolver[Unit]
}