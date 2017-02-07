package nutcracker.util

import scalaz.~>

/**
  *
  * @tparam O represents output. Examples: String, [[java.io.OutputStream]], ...
  * @tparam R represents type of data to be written to the output. Examples: String, bit string, ...
  * @tparam Ptr abstraction of pointers.
  */
trait ObjectOutput[O, R, Ptr[_]] {
  def write(out: O, r: R): O
  def writeSubObject[A, B](out: O, pa: Ptr[A])(f: A => B)(implicit ser: ObjectSerializer[B, R, Ptr]): O

  def writeObject[A](out: O, pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): O =
    writeSubObject(out, pa)(identity[A])
}

object ObjectOutput {

  def shallow[O, R, Ptr[_]](f: Ptr ~> λ[α => R])(implicit agg: Aggregator[O, R]): ObjectOutput[O, R, Ptr] =
    new ObjectOutput[O, R, Ptr] {
      def write(out: O, r: R): O = agg.append(out, r)
      def writeSubObject[A, B](out: O, pa: Ptr[A])(g: A => B)(implicit ser: ObjectSerializer[B, R, Ptr]): O = agg.append(out, f(pa))
    }

}