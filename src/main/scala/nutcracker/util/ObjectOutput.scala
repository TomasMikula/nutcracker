package nutcracker.util

import scala.annotation.tailrec
import scalaz.Id._
import scalaz.{-\/, Leibniz, Monoid, \/, \/-, ~>}

/**
  *
  * @tparam O represents output. Examples: String, [[java.io.OutputStream]], ...
  * @tparam R represents type of data to be written to the output. Examples: String, bit string, ...
  * @tparam Ptr abstraction of pointers.
  */
trait ObjectOutput[O, R, Ptr[_]] {
  def write(out: O, r: R): O
  def writeObject[A](out: O, pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): O
}

import FreeOutput.Mode

// isomorphic to List[R \/ (Ptr[A], ObjectSerializer[A, R, Ptr]) forSome { type A }]
sealed abstract class FreeOutput[R, Ptr[_], Mod <: Mode] {
  import FreeOutput._

  def reverse: FreeOutput[R, Ptr, Mod#Reverse] = {
    @tailrec def go(arg: FreeOutput[R, Ptr, Mod], acc: FreeOutput[R, Ptr, Mod#Reverse]): FreeOutput[R, Ptr, Mod#Reverse] =
      arg match {
        case Write(rest, s) => go(rest, Write(acc, s))
        case WriteObject(rest, pa, ser) => go(rest, WriteObject(acc, pa, ser))
        case Empty() => acc
      }

    go(this, empty)
  }

  /**
    *
    * @param s1 initial state
    * @param step given the accumulted state on the way down (`S1`) and a pointer (`Ptr[α]`),
    *             `step` has to decide whether
    *              - follow the pointer down, in which case it returns `Left` with new `S1` to
    *                be passed downwards and a function that
    *                 - transforms the state passed upwards (`S2`) and
    *                 - produces decoration for the result of recursion; or
    *              - the pointer can be handled directly, in which case it produces serialized
    *                representation of the pointer (`R`) and state to be passed upwards (`S2`).
    * @param S2 used to combine state passed up from siblings to state passed up by their parent.
    * @tparam S1 state to be passed downwards
    * @tparam S2 state to be passed upwards
    */
  def foldBiState[S1, S2](s1: S1)(step: S1 => Ptr ~> λ[α => (S1, S2 => (S2, Decoration[R])) \/ (S2, R)])(deref: Ptr ~> Id)(implicit S2: Monoid[S2], ev: Leibniz[Nothing, Mode, Mod, Append]): Lst[R] = {
    case class StackFrame(s1: S1, s2acc: S2, pred: Lst[R], tr: S2 => (S2, Decoration[R]), tail: FreeOutput[R, Ptr, Prepend])
    @tailrec def go(s1: S1, s2acc: S2, pred: Lst[R], tail: FreeOutput[R, Ptr, Prepend], stack: List[StackFrame]): Lst[R] =
      tail match {
        case Write(tail, r) => go(s1, s2acc, pred :+ r, tail, stack)
        case WriteObject(tail, pa, ser) => step(s1)(pa) match {
          case -\/((s11, tr)) => go(s11, S2.zero, Lst.empty, ser.write(FreeOutput.empty[R, Ptr, Append], deref(pa)).reverse, StackFrame(s1, s2acc, pred, tr, tail) :: stack)
          case \/-((s2, r)) => go(s1, S2.append(s2acc, s2), pred :+ r, tail, stack)
        }
        case Empty() => stack match {
          case StackFrame(s10, s20, ss, tr, tail) :: stack =>
            val (s2, dec) = tr(s2acc)
            val acc = dec match {
              case Naked => pred
              case BeforeAfter(before, after) => before +: pred :+ after
              case Before(r) => r +: pred
              case After(r) => pred :+ r
            }
            go(s10, S2.append(s20, s2), ss ++ acc, tail, stack)
          case Nil => pred
        }
      }

    go(s1, S2.zero, Lst.empty, ev.subst[({ type Out[m <: Mode] = FreeOutput[R, Ptr, m]})#Out](this).reverse, Nil)
  }

  /** Specialized [[foldBiState]] for `S2 = Unit`. */
  def foldState[S](s: S)(step: S => Ptr ~> λ[α => (S, Decoration[R]) \/ R])(deref: Ptr ~> Id)(implicit ev: Leibniz[Nothing, Mode, Mod, Append]): Lst[R] = {
    case class StackFrame(s: S, tail: FreeOutput[R, Ptr, Prepend])
    @tailrec def go(s: S, out: Lst[R], tail: FreeOutput[R, Ptr, Prepend], stack: List[StackFrame]): Lst[R] =
      tail match {
        case Write(tail, r) => go(s, out :+ r, tail, stack)
        case WriteObject(tail, pa, ser) => step(s)(pa) match {
          case -\/((s1, dec)) => go(s1, out :+? dec.beforeOption, ser.write(FreeOutput.empty[R, Ptr, Append], deref(pa)).reverse, StackFrame(s, dec.afterOption.map(Write(tail, _)).getOrElse(tail)) :: stack)
          case \/-(r) => go(s, out :+ r, tail, stack)
        }
        case Empty() => stack match {
          case StackFrame(s0, tail) :: stack => go(s0, out, tail, stack)
          case Nil => out
        }
      }

    go(s, Lst.empty, ev.subst[({ type Out[m <: Mode] = FreeOutput[R, Ptr, m]})#Out](this).reverse, Nil)
  }

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showPtr`. */
  def show(
    deref: Ptr ~> Id,
    decorateReferenced: Ptr ~> λ[α => Decoration[R]],
    decorateUnreferenced: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    E: HEqualK[Ptr],
    ev: Leibniz[Nothing, Mode, Mod, Append]
  ): Lst[R] = {
    type S1 = List[Ptr[_]] // context, i.e. all parents
    type S2 = Lst[Ptr[_]]  // referenced within the subgraph

    val step = (s1: S1) =>
      λ[Ptr ~> λ[α => (S1, S2 => (S2, Decoration[R])) \/ (S2, R)]](pa => {
        if(s1.exists(E.hEqual(_, pa))) \/-((Lst.singleton(pa), showReference(pa)))
        else -\/((pa :: s1, s2 => {
          val s21 = s2.filterNot(E.hEqual(_, pa))
          val dec = if(s21.size < s2.size) decorateReferenced(pa) else decorateUnreferenced(pa)
          (s21, dec)
        }))
      })

    foldBiState[S1, S2](Nil)(step)(deref)
  }

  /** Serialize, cutting off cycles. Pointers that would cause cycles will be handled by `showPtr`. */
  def show(
    deref: Ptr ~> Id,
    decorateContent: Ptr ~> λ[α => Decoration[R]],
    showReference: Ptr ~> λ[α => R]
  )(implicit
    E: HEqualK[Ptr],
    ev: Leibniz[Nothing, Mode, Mod, Append]
  ): Lst[R] = {
    type S = List[Ptr[_]] // context, i.e. all parents

    val step = (s: S) =>
      λ[Ptr ~> λ[α => (S, Decoration[R]) \/ R]](pa => {
        if(s.exists(E.hEqual(_, pa))) \/-(showReference(pa))
        else -\/((pa :: s, decorateContent(pa)))
      })

    foldState[S](Nil)(step)(deref)
  }
}

object FreeOutput {
  sealed trait Mode { type Reverse <: Mode }
  sealed trait Append extends Mode { type Reverse = Prepend }
  sealed trait Prepend extends Mode { type Reverse = Append }

  private case class Empty[R, Ptr[_], Mod <: Mode]() extends FreeOutput[R, Ptr, Mod]
  private case class Write[R, Ptr[_], Mod <: Mode](rest: FreeOutput[R, Ptr, Mod], r: R) extends FreeOutput[R, Ptr, Mod]
  private case class WriteObject[R, Ptr[_], Mod <: Mode, A](rest: FreeOutput[R, Ptr, Mod], pa: Ptr[A], ser: ObjectSerializer[A, R, Ptr]) extends FreeOutput[R, Ptr, Mod]

  sealed abstract class Decoration[+R] {
    def beforeOption: Option[R]
    def afterOption: Option[R]
  }
  case object Naked extends Decoration[Nothing] { def beforeOption = None; def afterOption = None }
  case class Before[R](r: R) extends Decoration[R] { def beforeOption = Some(r); def afterOption = None }
  case class After[R](r: R) extends Decoration[R] { def beforeOption = None; def afterOption = Some(r) }
  case class BeforeAfter[R](before: R, after: R) extends Decoration[R] { def beforeOption = Some(before); def afterOption = Some(after) }

  def empty[R, Ptr[_], Mod <: Mode]: FreeOutput[R, Ptr, Mod] = Empty()

  implicit def objectOutputInstance[R, Ptr[_]]: ObjectOutput[FreeOutput[R, Ptr, Append], R, Ptr] =
    new ObjectOutput[FreeOutput[R, Ptr, Append], R, Ptr] {
      def write(out: FreeOutput[R, Ptr, Append], r: R): FreeOutput[R, Ptr, Append] = Write(out, r)
      def writeObject[A](out: FreeOutput[R, Ptr, Append], pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): FreeOutput[R, Ptr, Append] =
        WriteObject(out, pa, ser)
    }
}