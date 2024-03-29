package nutcracker.util.ops

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.language.implicitConversions
import scalaz.{Applicative, Semigroup, ===}

object iterator extends ToIteratorOps

trait ToIteratorOps {
  implicit def toIteratorOps[A](it: Iterator[A]): IteratorOps[A] = IteratorOps(it)
}

final case class IteratorOps[A](it: Iterator[A]) extends AnyVal {
  import IteratorOps._

  def toMultiMap[K, V](implicit ev: A =:= (K, V)): Map[K, List[V]] = {
    val builder = MMap[K, List[V]]()
    while(it.hasNext) {
      val (k, v) = ev(it.next())
      builder.update(k, v :: builder.getOrElse(k, Nil))
    }
    builder.toMap
  }

  def collectToList[B](f: A => Option[B]): List[B] = {
    val buf = List.newBuilder[B]
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

  def sequence_[F[_], B](implicit ev: A === F[B], F: Applicative[F]): F[Unit] =
    traverse_(ev(_))

  def traverse_[F[_], B](f: A => F[B])(implicit F: Applicative[F]): F[Unit] =
    balancedMapReduce(f)(applicativeSemigroup[F, B]).fold(F.point(()))(F.map(_)(_ => ()))

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

  def balancedReduce(implicit A: Semigroup[A]): Option[A] =
    balancedMapReduce(identity[A])

  def balancedMapReduce[B](f: A => B)(implicit B: Semigroup[B]): Option[B] = {
    @tailrec def append(l: List[B], lFactor: Int, b: B): List[B] = {
      // |l| = lFactor * |b|
      //   where
      //     |l| is the total number of elements accumulated in (all elements of) l,
      //     |b| is the number of elements accumulated in b
      if (lFactor % 2 == 0) b :: l
      else append(l.tail, lFactor / 2, B.append(l.head, b))
    }

    var l = List[B]()
    var n = 0 // number of elements accumulated in l
    while(it.hasNext) {
      l = append(l, n, f(it.next()))
      n = n + 1
    }

    l.reduceLeftOption((acc, b) => B.append(b, acc))
  }
}

object IteratorOps {
  private[IteratorOps] def applicativeSemigroup[F[_], B](implicit F: Applicative[F]): Semigroup[F[B]] =
    new Semigroup[F[B]] {
      def append(f1: F[B], f2: => F[B]): F[B] = F.apply2(f1, f2)((_, a) => a)
    }
}