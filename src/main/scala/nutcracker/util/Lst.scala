package nutcracker.util

import scala.annotation.tailrec
import scalaz.Monoid

/** Linked list with O(1) cons, snoc and concatenation
  * and amortized O(1) uncons.
  */
sealed abstract class Lst[+A] {
  import Lst._

  @tailrec
  final def uncons: Option[(A, Lst[A])] = this match {
    case Nil => None
    case Cons(h, t) => Some((h, t))
    case Cat(l1, l2) => l1 match {
      case Cons(h, t) => Some((h, t ++ l2))
      case Cat(l11, l12) => (Cat(l11, Cat(l12, l2)): Lst[A]).uncons
    }
  }

  final def ::[B >: A](h: B): Lst[B] = Cons(h, this)

  final def rev_:::[B >: A](l: List[B]): Lst[B] =
    revPrepend(l, this)

  @inline
  final def ?+:[B >: A](b: Option[B]): Lst[B] = b match {
    case Some(x) => x :: this
    case None => this
  }

  final def ++[B >: A](that: Lst[B]): Lst[B] = this match {
    case Nil => that
    case this1: NELst[A] => that match {
      case Nil => this1
      case that1: NELst[B] => Cat[B](this1, that1)
    }
  }
}

object Lst {
  private final case object Nil extends Lst[Nothing]
  private sealed abstract class NELst[+A] extends Lst[A]
  private final case class Cons[A](h: A, t: Lst[A]) extends NELst[A]
  private final case class Cat[A](fst: NELst[A], snd: NELst[A]) extends NELst[A]

  @inline
  def empty[A]: Lst[A] = Nil

  @inline
  def singleton[A](a: A): Lst[A] = a :: Nil

  @inline
  def maybe[A](a: Option[A]): Lst[A] = a match {
    case Some(x) => x :: Nil
    case None => Nil
  }

  @inline
  private final def revPrepend[A](l: List[A], lst: Lst[A]): Lst[A] = l match {
    case a :: as => revPrepend(as, Cons(a, lst))
    case scala.Nil => lst
  }

  implicit def monoid[A]: Monoid[Lst[A]] = new Monoid[Lst[A]] {
    def zero: Lst[A] = Nil
    def append(f1: Lst[A], f2: => Lst[A]): Lst[A] = f1 ++ f2
  }
}