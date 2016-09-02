package nutcracker.util

import algebra.Eq
import algebra.lattice.GenBool
import algebra.instances.set._

case class TransformedIndex[K, VIn, VOut] private (
  private val keys: VIn => Seq[K],
  private val transform: (VIn, K) => VOut,
  private val index: Map[K, Set[VOut]]
) {
  import TransformedIndex._

  def add(v: VIn): TransformedIndex[K, VIn, VOut] = {
    copy(index = GenBool[Map[K, Set[VOut]]].join(index, singleEntryIndex(v)))
  }

  def remove(v: VIn): TransformedIndex[K, VIn, VOut] = {
    copy(index = GenBool[Map[K, Set[VOut]]].without(index, singleEntryIndex(v)))
  }

  def get(k: K): Set[VOut] = index.getOrElse(k, Set())

  private def singleEntryIndex(v: VIn): Map[K, Set[VOut]] =
    (keys(v) map { k => (k, Set(transform(v, k))) }).toMap
}

object TransformedIndex {
  def empty[K, VIn, VOut](keys: VIn => Seq[K], transform: (VIn, K) => VOut): TransformedIndex[K, VIn, VOut] =
    TransformedIndex(keys, transform, Map.empty)

  private implicit def mapGenBool[K, V](implicit GBV: GenBool[V], EqV: Eq[V]): GenBool[Map[K, V]] = new GenBool[Map[K, V]] {
    def zero: Map[K, V] = Map.empty

    def and(a: Map[K, V], b: Map[K, V]): Map[K, V] = (
      (a.keySet intersect b.keySet).iterator
        map { k => (k, GBV.and(a(k), b(k))) }
        filter { case (k, v) => EqV.neqv(GBV.zero, v) }
      ).toMap

    def or(a: Map[K, V], b: Map[K, V]): Map[K, V] = (
      (a.keySet union b.keySet).iterator
        map { k => (k, GBV.or(a.getOrElse(k, GBV.zero), b.getOrElse(k, GBV.zero))) }
      ).toMap

    def without(a: Map[K, V], b: Map[K, V]): Map[K, V] = (
      a.iterator map { case (k, v) => b.get(k) match {
        case Some(v2) => (k, GBV.without(v, v2))
        case None => (k, v)
      }}
        filter { case (k, v) => EqV.neqv(GBV.zero, v) }
      ).toMap
  }
}