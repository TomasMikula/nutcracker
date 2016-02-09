package nutcracker.util

import algebra.lattice.GenBool
import algebra.std.map._
import algebra.std.set._

case class TransformedIndex[K, VIn, VOut] private (
  private val keys: VIn => Seq[K],
  private val transform: (VIn, K) => VOut,
  private val index: Map[K, Set[VOut]]
) {

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
}