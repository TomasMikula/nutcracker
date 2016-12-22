package nutcracker.util

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
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

  final def isEmpty: Boolean = this match {
    case Nil => true
    case _ => false
  }

  final def size: Int =
    foldLeft(0)((n, _) => n + 1)

  final def ::[B >: A](h: B): Lst[B] = Cons(h, this)

  final def +:[B >: A](b: B): Lst[B] = Cons(b, this)
  final def :+[B >: A](b: B): Lst[B] = this ++ singleton(b)

  final def rev_:::[B >: A](bs: Iterable[B]): Lst[B] =
    revPrepend(bs, this)

  final def mapRev_:::[B, A1 >: A](bsf: (Iterable[B], B => A1)): Lst[A1] =
    mapRevPrepend(bsf._1, this, bsf._2)

  @inline
  final def ?+:[B >: A](b: Option[B]): Lst[B] = b match {
    case Some(x) => x +: this
    case None => this
  }

  @inline
  final def :+?[B >: A](b: Option[B]): Lst[B] = b match {
    case Some(x) => this :+ x
    case None => this
  }

  final def ++[B >: A](that: Lst[B]): Lst[B] = this match {
    case Nil => that
    case this1: NELst[A] => that match {
      case Nil => this1
      case that1: NELst[B] => Cat[B](this1, that1)
    }
  }

  final def toRevList: List[A] = {
    @tailrec def go(as: Lst[A], acc: List[A]): List[A] = as.toRightAssoc match {
      case Nil => acc
      case Cons(h, t) => go(t, h :: acc)
      case Cat(Cons(h, t1), t2) => go(t1 ++ t2, h :: acc)
      case _ => sys.error("Unreachable code")
    }
    go(this, List.empty)
  }

  final def toList: List[A] = {
    val b = List.newBuilder[A]

    @tailrec def go(as: Lst[A]): List[A] = as.toRightAssoc match {
      case Nil => b.result()
      case Cons(h, t) => b += h; go(t)
      case Cat(Cons(h, t1), t2) => b += h; go(t1 ++ t2)
      case _ => sys.error("Unreachable code")
    }

    go(this)
  }

  final def map[B](f: A => B): Lst[B] = {
    val buf = Buffer.empty[B]

    @tailrec def go(as: Lst[A]): Lst[B] = as.toRightAssoc match {
      case Nil => buf.foldRight(Lst.empty[B])((b, l) => b :: l)
      case Cons(h, t) => buf += f(h); go(t)
      case Cat(Cons(h, t1), t2) => buf += f(h); go(t1 ++ t2)
      case _ => sys.error("Unreachable code")
    }

    go(this)
  }

  final def filter(p: A => Boolean): Lst[A] =
    foldLeft(Buffer[A]())((buf, a) => if (p(a)) (buf += a) else buf).foldRight(Lst.empty[A])((a, l) => a :: l)

  final def filterNot(p: A => Boolean): Lst[A] =
    filter(!p(_))

  final def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def go(acc: B, cur: Lst[A], tail: List[Lst[A]]): B = cur match {
      case Cat(y, z) => go(acc, y, z :: tail)
      case Cons(x, y) => go(f(acc, x), y, tail)
      case Nil => tail match {
        case l :: ls => go(acc, l, ls)
        case nil => acc
      }
    }

    go(b, this, List())
  }

  @tailrec
  private final def toRightAssoc: Lst[A] = this match {
    case Cat(Cat(x, y), z) => (Cat(x, Cat(y, z)): Lst[A]).toRightAssoc
    case l => l
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
  private final def revPrepend[A](as: Iterable[A], lst: Lst[A]): Lst[A] = {
    @tailrec def loop(it: Iterator[A], lst: Lst[A]): Lst[A] =
      if(it.hasNext) loop(it, Cons(it.next, lst))
      else lst

    loop(as.iterator, lst)
  }

  @inline
  private final def mapRevPrepend[A, B](as: Iterable[A], lst: Lst[B], f: A => B): Lst[B] = {
    @tailrec def loop(it: Iterator[A], lst: Lst[B]): Lst[B] =
      if(it.hasNext) loop(it, Cons(f(it.next), lst))
      else lst

    loop(as.iterator, lst)
  }

  implicit def monoid[A]: Monoid[Lst[A]] = new Monoid[Lst[A]] {
    def zero: Lst[A] = Nil
    def append(f1: Lst[A], f2: => Lst[A]): Lst[A] = f1 ++ f2
  }
}
