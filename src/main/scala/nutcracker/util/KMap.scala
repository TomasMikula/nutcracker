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
  def getOrElse[A](k: K[A], default: => V[A]): V[A] = get(k).getOrElse(default)
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

/** KMap with an upper bound on the type parameter accepted by K[_], V[_]. */
final case class KMapB[K[_ <: UB], V[_ <: UB], UB](map: Map[K[_], V[_]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def head: (K[A], V[A]) forSome { type A <: UB } = map.head.asInstanceOf[(K[A], V[A]) forSome { type A <: UB }]
  def tail: KMapB[K, V, UB] = KMapB[K, V, UB](map.tail)
  def apply[A <: UB](k: K[A]): V[A] = map(k).asInstanceOf[V[A]]
  def get[A <: UB](k: K[A]): Option[V[A]] = map.get(k).asInstanceOf[Option[V[A]]]
  def getOrElse[A <: UB](k: K[A], default: => V[A]): V[A] = get(k).getOrElse(default)
  def updated[A <: UB](k: K[A], v: V[A]): KMapB[K, V, UB] = KMapB[K, V, UB](map.updated(k, v))
  def updated[A <: UB](k: K[A], v: V[A], combineIfPresent: (V[A], V[A]) => V[A]): KMapB[K, V, UB] =
    get(k) match {
      case None => updated(k, v)
      case Some(v0) => updated(k, combineIfPresent(v0, v))
    }
  def -(k: K[_]): KMapB[K, V, UB] = KMapB[K, V, UB](map - k)
}

object KMapB {
  def apply[K[_], V[_], UB](): KMapB[K, V, UB] = KMapB[K, V, UB](Map[K[_], V[_]]())
}


final case class K2Map[K[_, _], V[_, _]](map: Map[K[_, _], V[_, _]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def head: (K[A, B], V[A, B]) forSome { type A; type B } = map.head.asInstanceOf[(K[A, B], V[A, B]) forSome { type A; type B }]
  def tail: K2Map[K, V] = K2Map[K, V](map.tail)
  def apply[A, B](k: K[A, B]): V[A, B] = map(k).asInstanceOf[V[A, B]]
  def get[A, B](k: K[A, B]): Option[V[A, B]] = map.get(k).asInstanceOf[Option[V[A, B]]]
  def getOrElse[A, B](k: K[A, B], default: => V[A, B]): V[A, B] = get(k).getOrElse(default)
  def updated[A, B](k: K[A, B], v: V[A, B]): K2Map[K, V] = K2Map[K, V](map.updated(k, v))
  def updated[A, B](k: K[A, B], v: V[A, B], combineIfPresent: (V[A, B], V[A, B]) => V[A, B]): K2Map[K, V] =
    get(k) match {
      case None => updated(k, v)
      case Some(v0) => updated(k, combineIfPresent(v0, v))
    }
  def -(k: K[_, _]): K2Map[K, V] = K2Map[K, V](map - k)
}

object K2Map {
  def apply[K[_, _], V[_, _]](): K2Map[K, V] = K2Map[K, V](Map[K[_, _], V[_, _]]())
}