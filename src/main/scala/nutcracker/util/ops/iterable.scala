package nutcracker.util.ops

import scala.language.implicitConversions

object iterable extends ToIterableOps

trait ToIterableOps {
  implicit def toIterableOps[A](col: Iterable[A]): IterableOps[A] = IterableOps(col)
}

final case class IterableOps[A](col: Iterable[A]) extends AnyVal {
  def toMultiMap[K, V](implicit ev: A =:= (K, V)): Map[K, List[V]] =
    col.iterator.toMultiMap

  def collectToList[B](f: A => Option[B]): List[B] =
    col.iterator.collectToList(f)
}