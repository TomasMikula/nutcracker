package nutcracker.util

import nutcracker.util.FreeObjectOutput.Decoration

import scalaz.Id.Id
import scalaz.Leibniz.===
import scalaz.{Show, ~>}

/** Serialization of (potentially cyclic) object graphs.
  * Features:
  *  - abstracted over pointers;
  *  - termination and correctness in presence of cycles;
  *  - stack safety.
  *
  * @tparam A type of objects that this serializer can serialize.
  * @tparam S type of data this serializer writes when serializing `A`, e.g. `String`, byte array, etc.
  * @tparam Ptr abstraction of pointers.
  */
trait ObjectSerializer[A, S, Ptr[_]] { self =>

  def write[O](out: O, a: A)(implicit ev: ObjectOutput[O, S, Ptr]): O

  def serialize[M[_]](a: A)(implicit ev: MonadObjectOutput[M, S, Ptr]): M[Unit]

  def pointer: ObjectSerializer[Ptr[A], S, Ptr]

  final def free(a: A): FreeObjectOutput[S, Ptr, Unit] = serialize[FreeObjectOutput[S, Ptr, *]](a)

  def show(deref: Ptr ~> Id, showRef: Ptr ~> λ[α => String])(
    decorateReferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration(s"<def ${showRef(ref)}>", "</def>")),
    decorateUnreferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration("", "")),
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit E: HEqualK[Ptr], ev: S === String): Show[A] = new Show[A] {
    override def shows(a: A): String =
      self.free(a).showAutoLabeled(deref, showRef)(decorateReferenced, decorateUnreferenced, decorateReference)
  }

  def shallowShow(implicit S: ShowK[Ptr], ev: S === String): Show[A] = new Show[A] {
    override def shows(a: A): String = ev.subst[ObjectSerializer[A, *, Ptr]](self).free(a).showShallow(S)
  }
}

object ObjectSerializer {

  trait FromWrite[A, S, Ptr[_]] extends ObjectSerializer[A, S, Ptr] {
    def serialize[M[_]](a: A)(implicit ev: MonadObjectOutput[M, S, Ptr]): M[Unit] =
      write(ev.point(()), a)(ev.objectOutput)

    def pointer: ObjectSerializer[Ptr[A], S, Ptr] = new FromWrite[Ptr[A], S, Ptr] {
      def write[O](out: O, pa: Ptr[A])(implicit ev: ObjectOutput[O, S, Ptr]): O =
        ev.writeObject(out, pa)(FromWrite.this)
    }
  }

  trait FromSerialize[A, S, Ptr[_]] extends ObjectSerializer[A, S, Ptr] {
    def write[O](out: O, a: A)(implicit ev: ObjectOutput[O, S, Ptr]): O =
      free(a).writeTo(out)

    def pointer: ObjectSerializer[Ptr[A], S, Ptr] = new FromSerialize[Ptr[A], S, Ptr] {
      def serialize[M[_]](pa: Ptr[A])(implicit ev: MonadObjectOutput[M, S, Ptr]): M[Unit] =
        ev.writeRec(pa)(a => FromSerialize.this.serialize(a))
    }
  }

  implicit def specialize[A[_[_]], Ptr[_]](implicit ev: DeepShowK[A]): DeepShow[A[Ptr], Ptr] =
    ev.specialize[Ptr]

}