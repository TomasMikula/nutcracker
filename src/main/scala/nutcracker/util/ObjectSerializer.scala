package nutcracker.util

import scalaz.BindRec

/**
  *
  * @tparam A type of objects that this serializer can serialize.
  * @tparam S type of data this serializer writes when serializing `A`.
  * @tparam Ptr abstraction of pointers.
  */
trait ObjectSerializer[A, S, Ptr[_]] {
  def write[O](out: O, a: A)(implicit ev: ObjectOutput[O, S, Ptr]): O

  def serialize[M[_]](a: A)(implicit ev: MonadObjectOutput[M, S, Ptr], M1: BindRec[M]): M[Unit]
}

object ObjectSerializer {

  trait FromWrite[A, S, Ptr[_]] extends ObjectSerializer[A, S, Ptr] {
    def serialize[M[_]](a: A)(implicit ev: MonadObjectOutput[M, S, Ptr], M1: BindRec[M]): M[Unit] =
      write(ev.point(()), a)(ev.objectOutput)
  }

  trait FromSerialize[A, S, Ptr[_]] extends ObjectSerializer[A, S, Ptr] {
    def write[O](out: O, a: A)(implicit ev: ObjectOutput[O, S, Ptr]): O =
      serialize[FreeObjectOutput[S, Ptr, ?]](a).writeTo(out)
  }

}