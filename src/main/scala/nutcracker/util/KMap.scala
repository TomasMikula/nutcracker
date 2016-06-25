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
  def getOrElse[A](k: K[A])(default: => V[A]): V[A] = get(k).getOrElse(default)
  def put[A](k: K[A])(v: V[A]): KMap[K, V] = KMap[K, V](map.updated(k, v))
  def updated[A](k: K[A])(v: V[A])(combineIfPresent: (V[A], V[A]) => V[A]): KMap[K, V] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
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
  def getOrElse[A <: UB](k: K[A])(default: => V[A]): V[A] = get(k).getOrElse(default)
  def put[A <: UB](k: K[A])(v: V[A]): KMapB[K, V, UB] = KMapB[K, V, UB](map.updated(k, v))
  def updated[A <: UB](k: K[A])(v: V[A])(combineIfPresent: (V[A], V[A]) => V[A]): KMapB[K, V, UB] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
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
  def getOrElse[A, B](k: K[A, B])(default: => V[A, B]): V[A, B] = get(k).getOrElse(default)
  def put[A, B](k: K[A, B])(v: V[A, B]): K2Map[K, V] = K2Map[K, V](map.updated(k, v))
  def updated[A, B](k: K[A, B])(v: V[A, B])(combineIfPresent: (V[A, B], V[A, B]) => V[A, B]): K2Map[K, V] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
    }
  def -(k: K[_, _]): K2Map[K, V] = K2Map[K, V](map - k)
}

object K2Map {
  def apply[K[_, _], V[_, _]](): K2Map[K, V] = K2Map[K, V](Map[K[_, _], V[_, _]]())
}


final case class K3Map[K[_, _, _], V[_, _, _]](map: Map[K[_, _, _], V[_, _, _]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def head: K3Pair[K, V] = K3Pair(map.head.asInstanceOf[(K[A, B, C], V[A, B, C]) forSome { type A; type B; type C }])
  def tail: K3Map[K, V] = K3Map[K, V](map.tail)
  def apply[A, B, C](k: K[A, B, C]): V[A, B, C] = map(k).asInstanceOf[V[A, B, C]]
  def get[A, B, C](k: K[A, B, C]): Option[V[A, B, C]] = map.get(k).asInstanceOf[Option[V[A, B, C]]]
  def getOrElse[A, B, C](k: K[A, B, C])(default: => V[A, B, C]): V[A, B, C] = get(k).getOrElse(default)
  def put[A, B, C](k: K[A, B, C])(v: V[A, B, C]): K3Map[K, V] = K3Map[K, V](map.updated(k, v))
  def updated[A, B, C](k: K[A, B, C])(v: V[A, B, C])(combineIfPresent: (V[A, B, C], V[A, B, C]) => V[A, B, C]): K3Map[K, V] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
    }
  def -(k: K[_, _, _]): K3Map[K, V] = K3Map[K, V](map - k)
}

object K3Map {
  def apply[K[_, _, _], V[_, _, _]](): K3Map[K, V] = K3Map[K, V](Map[K[_, _, _], V[_, _, _]]())
}

sealed abstract class K3Pair[K[_, _, _], V[_, _, _]] {
  type A; type B; type C
  val _1: K[A, B, C]
  val _2: V[A, B, C]
}

object K3Pair {

  def apply[K[_, _, _], V[_, _, _], A0, B0, C0](p: (K[A0, B0, C0], V[A0, B0, C0])): K3Pair[K, V] =
    K3Pair(p._1, p._2)

  def apply[K[_, _, _], V[_, _, _], A0, B0, C0](k: K[A0, B0, C0], v: V[A0, B0, C0]): K3Pair[K, V] =
    new K3Pair[K, V] {
      type A = A0; type B = B0; type C = C0
      val _1 = k
      val _2 = v
    }
}
