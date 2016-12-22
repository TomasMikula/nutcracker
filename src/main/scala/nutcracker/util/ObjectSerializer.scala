package nutcracker.util

/**
  *
  * @tparam A type of objects that this serializer can serialize.
  * @tparam S type of data this serializer writes when serializing `A`.
  * @tparam Ptr abstraction of pointers.
  */
trait ObjectSerializer[A, S, Ptr[_]] {
  def write[O](out: O, a: A)(implicit ev: ObjectOutput[O, S, Ptr]): O
}