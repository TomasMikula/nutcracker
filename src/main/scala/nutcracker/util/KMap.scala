package nutcracker.util

import nutcracker.util.typealigned.APair

import scala.language.existentials
import scala.language.higherKinds
import scalaz.~>

final case class KMap[K[_], V[_]](map: Map[K[_], V[_]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
  def head: ∃[λ[α => (K[α], V[α])]] = map.head.asInstanceOf[∃[λ[α => (K[α], V[α])]]]
  def tail: KMap[K, V] = KMap[K, V](map.tail)
  def apply[A](k: K[A]): V[A] = map(k).asInstanceOf[V[A]]
  def get[A](k: K[A]): Option[V[A]] = map.get(k).asInstanceOf[Option[V[A]]]
  def find(p: ∃[V] => Boolean): Option[∃[λ[α => (K[α], V[α])]]] = map.find(kv => p(kv._2)).asInstanceOf[Option[∃[λ[α => (K[α], V[α])]]]]
  def getOrElse[A](k: K[A])(default: => V[A]): V[A] = get(k).getOrElse(default)
  def put[A](k: K[A])(v: V[A]): KMap[K, V] = KMap[K, V](map.updated(k, v))
  def updated[A](k: K[A])(v: V[A])(combineIfPresent: (V[A], V[A]) => V[A]): KMap[K, V] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
    }
  def -(k: K[_]): KMap[K, V] = KMap[K, V](map - k)
  def mapValues[W[_]](f: V ~> W): KMap[K, W] =
    KMap[K, W](map.mapValues[W[_]](v => f(v)))
  def ++(that: KMap[K, V]): KMap[K, V] =
    KMap[K, V](this.map ++ that.map)
  def toStream: Stream[APair[K, V]] =
    map.toStream.map(kv => APair(kv._1.asInstanceOf[K[Any]], kv._2.asInstanceOf[V[Any]]))
}

object KMap {
  def apply[K[_], V[_]](): KMap[K, V] = KMap[K, V](Map[K[_], V[_]]())
}

final case class HKMap[K[_[_]], V[_[_]]](map: Map[K[Any], V[Any]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
  def head: (K[A], V[A]) forSome { type A[_] } = map.head.asInstanceOf[(K[A], V[A]) forSome { type A[_] }]
  def tail: HKMap[K, V] = HKMap[K, V](map.tail)
  def apply[A[_]](k: K[A]): V[A] = map(k.asInstanceOf[K[Any]]).asInstanceOf[V[A]]
  def get[A[_]](k: K[A]): Option[V[A]] = map.get(k.asInstanceOf[K[Any]]).asInstanceOf[Option[V[A]]]
  def getOrElse[A[_]](k: K[A])(default: => V[A]): V[A] = get(k).getOrElse(default)
  def put[A[_]](k: K[A])(v: V[A]): HKMap[K, V] = HKMap[K, V](map.updated(k.asInstanceOf[K[Any]], v.asInstanceOf[V[Any]]))
  def updated[A[_]](k: K[A])(v: V[A])(combineIfPresent: (V[A], V[A]) => V[A]): HKMap[K, V] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
    }
  def -(k: K[Any]): HKMap[K, V] = HKMap[K, V](map - k)
}

object HKMap {
  def apply[K[_[_]], V[_[_]]](): HKMap[K, V] = HKMap[K, V](Map[K[Any], V[Any]]())
}

final case class HHKMap[K[_[_[_]]], V[_[_[_]]]](map: Map[K[Any], V[Any]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
  def head: (K[A], V[A]) forSome { type A[_[_]] } = map.head.asInstanceOf[(K[A], V[A]) forSome { type A[_[_]] }]
  def tail: HHKMap[K, V] = HHKMap[K, V](map.tail)
  def apply[A[_[_]]](k: K[A]): V[A] = map(k.asInstanceOf[K[Any]]).asInstanceOf[V[A]]
  def get[A[_[_]]](k: K[A]): Option[V[A]] = map.get(k.asInstanceOf[K[Any]]).asInstanceOf[Option[V[A]]]
  def getOrElse[A[_[_]]](k: K[A])(default: => V[A]): V[A] = get(k).getOrElse(default)
  def put[A[_[_]]](k: K[A])(v: V[A]): HHKMap[K, V] = HHKMap[K, V](map.updated(k.asInstanceOf[K[Any]], v.asInstanceOf[V[Any]]))
  def updated[A[_[_]]](k: K[A])(v: V[A])(combineIfPresent: (V[A], V[A]) => V[A]): HHKMap[K, V] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
    }
  def -(k: K[Any]): HHKMap[K, V] = HHKMap[K, V](map - k)
}

object HHKMap {
  def apply[K[_[_[_]]], V[_[_[_]]]](): HHKMap[K, V] = HHKMap[K, V](Map[K[Any], V[Any]]())
}

/** KMap with an upper bound on the type parameter accepted by K[_], V[_]. */
final case class KMapB[K[_ <: UB], V[_ <: UB], UB](map: Map[K[_ <: UB], V[_ <: UB]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
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
  def -(k: K[_ <: UB]): KMapB[K, V, UB] = KMapB[K, V, UB](map - k)
}

object KMapB {
  def apply[K[_ <: UB], V[_ <: UB], UB](): KMapB[K, V, UB] = KMapB[K, V, UB](Map[K[_ <: UB], V[_ <: UB]]())
}


/** KMap where values are parameterized by 2 additional type parameters,
  * uniquely determined by a typeclass instance. Type safety relies on
  * `TC[A, B, C]` being functional in `A`, i.e. for each `A` there are
  * globally unique `B`, `C` such that there is an instance of `TC[A, B, C]`.
  */
final case class KMap1_2[K[_], V[_, _, _], TC[_, _, _]](map: Map[K[_], V[_, _, _]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
  def head: (K[A], V[A, B, C]) forSome { type A; type B; type C } = map.head.asInstanceOf[(K[A], V[A, B, C]) forSome { type A; type B; type C }]
  def tail: KMap1_2[K, V, TC] = KMap1_2[K, V, TC](map.tail)
  def apply[A, B, C](k: K[A])(implicit ev: TC[A, B, C]): V[A, B, C] = map(k).asInstanceOf[V[A, B, C]]
  def get[A, B, C](k: K[A])(implicit ev: TC[A, B, C]): Option[V[A, B, C]] = map.get(k).asInstanceOf[Option[V[A, B, C]]]
  def getOrElse[A, B, C](k: K[A])(default: => V[A, _, _])(implicit ev: TC[A, B, C]): V[A, B, C] = get(k).getOrElse(default.asInstanceOf[V[A, B, C]])
  def put[A, B, C](k: K[A])(v: V[A, B, C])(implicit ev: TC[A, B, C]): KMap1_2[K, V, TC] = KMap1_2[K, V, TC](map.updated(k, v))
  def updated[A, B, C](k: K[A])(v: V[A, B, C])(combineIfPresent: (V[A, B, C], V[A, B, C]) => V[A, B, C])(implicit ev: TC[A, B, C]): KMap1_2[K, V, TC] =
    get(k) match {
      case None => put(k)(v)
      case Some(v0) => put(k)(combineIfPresent(v0, v))
    }
  def -(k: K[_]): KMap1_2[K, V, TC] = KMap1_2[K, V, TC](map - k)
}

object KMap1_2 {
  def apply[K[_], V[_, _, _], TC[_, _, _]](): KMap1_2[K, V, TC] = KMap1_2[K, V, TC](Map[K[_], V[_, _, _]]())
}


final case class K2Map[K[_, _], V[_, _]](map: Map[K[_, _], V[_, _]]) extends AnyVal {
  def isEmpty: Boolean = map.isEmpty
  def nonEmpty: Boolean = map.nonEmpty
  def size: Int = map.size
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
  def size: Int = map.size
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
