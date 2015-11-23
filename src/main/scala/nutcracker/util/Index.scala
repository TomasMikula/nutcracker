package nutcracker.util

import algebra.lattice.GenBool
import algebra.std.map._
import algebra.std.set._

case class Index[V, K] private (private val f: V => Seq[K], private val index: Map[K, Set[V]]) {

  def add(v: V): Index[V, K] = {
    copy(index = GenBool[Map[K, Set[V]]].join(index, singletonMap(v)))
  }

  def remove(v: V): Index[V, K] = {
    copy(index = GenBool[Map[K, Set[V]]].without(index, singletonMap(v)))
  }

  def get(k: K): Set[V] = index.getOrElse(k, Set())

  private def singletonMap(v: V): Map[K, Set[V]] =
    (f(v) map { (_, Set(v)) }).toMap
}

object Index {
  def empty[V, K](f: V => Seq[K]): Index[V, K] = Index(f, Map.empty)
}