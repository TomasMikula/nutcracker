package nutcracker

import scala.language.higherKinds
import nutcracker.Assessment._
import nutcracker.util.free.FreeK

import scala.annotation.tailrec
import scalaz.Free.Trampoline
import scalaz.Id._
import scalaz.{Monoid, StreamT, ~>}
import scalaz.syntax.applicative._

class DFSSolver[C: Monoid] {

  val lang: PropRelCost[C] = new PropRelCost[C]

  type K[A] = FreeK[lang.Vocabulary, A]
  type S = lang.State[K]

  def solutions[A](p: K[Promised[A]]): StreamT[Id, A] = {
    val (s, pr) = init(p)
    solutions(s) map { s => lang.propStore.get(s).fetchResult(pr).get }
  }

  def solutions1[A](p: K[Promised[A]]): StreamT[Id, (Option[A], Int)] = {
    val (s, pr) = init(p)
    solutions1(s) map { case (so, n) => (so map { lang.propStore.get(_).fetchResult(pr).get }, n) }
  }

  def allSolutions1[A](p: K[Promised[A]]): (List[A], Int) = {
    solutions1(p).foldLeft((List[A](), 0)) {
      case ((sols, n), (Some(sol), i)) => (sol :: sols, n+i)
      case ((sols, n), (None, i)) => (sols, n+i)
    }
  }

  def assess: S => Assessment[List[K[Unit]]] = lang.naiveAssess

  private def init[A](p: K[Promised[A]]): (S, Promised[A]) = {
    lang.interpreter.runFree(p)
  }

  private def solutions(s: S): StreamT[Id, S] =
    solutions[S](s, StreamT.empty[Id, S], (_: S) :: StreamT.empty[Id, S])

  private def solutions1(ps: S): StreamT[Id, (Option[S], Int)] = {
    val leafs = solutions[Option[S]](ps, None :: StreamT.empty[Id, Option[S]], Some(_: S) :: StreamT.empty[Id, Option[S]])

    def takeOne(s: StreamT[Id, Option[S]]): Option[((Option[S], Int), StreamT[Id, Option[S]])] = takeOne0(s, 0)

    @tailrec def takeOne0(s: StreamT[Id, Option[S]], acc: Int): Option[((Option[S], Int), StreamT[Id, Option[S]])] = s.uncons match {
      case Some((Some(a), tail)) => Some(((Some(a), acc), tail))
      case Some((None, tail)) => takeOne0(tail, acc+1)
      case None =>
        if(acc > 0) Some(((None, acc), StreamT.empty))
        else None
    }

    StreamT.unfold[(Option[S], Int), StreamT[Id, Option[S]]](leafs){ takeOne(_) }
  }

  private def solutions[B](
    s: S,
    failed: StreamT[Id, B],
    done: S => StreamT[Id, B]
  ): StreamT[Id, B] =
    hideTrampoline(solutionsT(s, failed, done))

  private def solutionsT[B](
    s: S,
    failed: StreamT[Id, B],
    done: S => StreamT[Id, B]): StreamT[Trampoline, B] =
    assess(s) match {
      case Failed => failed.trans(trampolineId)
      case Stuck => failed.trans(trampolineId) // TODO: Don't treat as failed.
      case Done => done(s).trans(trampolineId)
      case Incomplete(branches) =>
        StreamT.fromIterable(branches).trans(trampolineId) flatMap { k => solutionsT(lang.interpreter.runFreeUnit(s, k), failed, done) }
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