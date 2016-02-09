package nutcracker

package object util {

  type Index[K, V] = TransformedIndex[K, V, V]

  object Index {
    def empty[K, V](f: V => Seq[K]): Index[K, V] = TransformedIndex.empty(f, (v, k) => v)
  }

}
