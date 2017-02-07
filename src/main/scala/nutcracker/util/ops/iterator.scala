package nutcracker.util.ops

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions
import scalaz.Leibniz.===
import scalaz.{Applicative, Semigroup}

object iterator extends ToIteratorOps

trait ToIteratorOps {
  implicit def toIteratorOps[A](it: Iterator[A]): IteratorOps[A] = IteratorOps(it)
}

final case class IteratorOps[A](it: Iterator[A]) extends AnyVal {
  def toMultiMap[K, V](implicit ev: A =:= (K, V)): Map[K, List[V]] = {
    val builder = MMap[K, List[V]]()
    while(it.hasNext) {
      val (k, v) = ev(it.next())
      builder.update(k, v :: builder.getOrElse(k, Nil))
    }
    builder.toMap
  }

  def collectToList[B](f: A => Option[B]): List[B] = {
    var buf = List.newBuilder[B]
    while(it.hasNext) {
      f(it.next()).foreach(buf += _)
    }
    buf.result()
  }

  def mapFilter[B](f: A => Option[B]): Iterator[B] = {
    it.flatMap(a => f(a) match {
      case Some(b) => Iterator.single(b)
      case None => Iterator.empty
    })
  }

  def sequence_[F[_]](implicit F: Applicative[F], ev: A === F[Unit]): F[Unit] =
    ev.subst[IteratorOps](this).balancedReduce(new Semigroup[F[Unit]] {
      def append(f1: F[Unit], f2: => F[Unit]): F[Unit] = F.apply2(f1, f2)((_, _) => ())
    }).getOrElse(F.point(()))

  def intersperse(a: A): Iterator[A] = new Iterator[A] {
    var nextIsSeparator: Boolean = false

    def hasNext: Boolean = it.hasNext

    def next(): A = {
      if(nextIsSeparator) {
        nextIsSeparator = false
        a
      } else {
        nextIsSeparator = true
        it.next()
      }
    }
  }

  def interleave(that: Iterator[A]): Iterator[A] = new Iterator[A] {
    var fst: Iterator[A] = if(it.hasNext) it else that
    var snd: Iterator[A] = that

    def hasNext: Boolean = fst.hasNext

    def next(): A = {
      val res = fst.next()
      if(snd.hasNext) {
        val tmp = fst
        fst = snd
        snd = tmp
      }
      res
    }
  }

  def balancedReduce(implicit A: Semigroup[A]): Option[A] = {
    @tailrec def append(l: List[A], lFactor: Int, a: A): List[A] = {
      // |l| = lFactor * |a|
      //   where
      //     |l| is the total number of elements accumulated in (all elements of) l,
      //     |a| number of elements accumulated in a
      if (lFactor % 2 == 0) a :: l
      else append(l.tail, lFactor / 2, A.append(l.head, a))
    }

    var l = List[A]()
    var n = 0 // number of elements accumulated in l
    while(it.hasNext) {
      l = append(l, n, it.next)
      n = n + 1
    }

    l.reduceLeftOption((acc, a) => A.append(a, acc))
  }
}