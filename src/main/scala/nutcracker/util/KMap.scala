package nutcracker.util

import scala.language.existentials
import scala.language.higherKinds

final case class KMap[K[_], V[_]](map: Map[K[_], V[_]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def head: (K[A], V[A]) forSome { type A } = map.head.asInstanceOf[(K[A], V[A]) forSome { type A }]
  def tail: KMap[K, V] = KMap[K, V](map.tail)
  def apply[A](k: K[A]): V[A] = map(k).asInstanceOf[V[A]]
  def get[A](k: K[A]): Option[V[A]] = map.get(k).asInstanceOf[Option[V[A]]]
  def updated[A](k: K[A], v: V[A]): KMap[K, V] = KMap[K, V](map.updated(k, v))
  def updated[A](k: K[A], v: V[A], combineIfPresent: (V[A], V[A]) => V[A]): KMap[K, V] =
    get(k) match {
      case None => updated(k, v)
      case Some(v0) => updated(k, combineIfPresent(v0, v))
    }
  def -(k: K[_]): KMap[K, V] = KMap[K, V](map - k)
}

object KMap {
  def apply[K[_], V[_]](): KMap[K, V] = KMap[K, V](Map[K[_], V[_]]())
}
