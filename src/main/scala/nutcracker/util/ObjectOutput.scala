package nutcracker.util

/**
  *
  * @tparam O represents output. Examples: String, [[java.io.OutputStream]], ...
  * @tparam R represents type of data to be written to the output. Examples: String, bit string, ...
  * @tparam Ptr abstraction of pointers.
  */
trait ObjectOutput[O, R, Ptr[_]] {
  def write(out: O, r: R): O
  def writeObject[A](out: O, pa: Ptr[A])(implicit ser: ObjectSerializer[A, R, Ptr]): O
}