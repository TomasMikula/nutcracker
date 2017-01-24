package nutcracker.util

import nutcracker.util.FreeObjectOutput.Decoration

import scalaz.{BindRec, Show, ~>}
import scalaz.Id._

/** Printing of (potentially cyclic) object graphs.
  * Features:
  *  - abstracted over pointers;
  *  - termination and correctness in presence of cycles;
  *  - stack safety.
  */
trait DeepShow[A, Ptr[_]] extends ObjectSerializer[A, String, Ptr] {
  def show(a: A): Desc[Ptr]

  def toShow(deref: Ptr ~> Id, showRef: Ptr ~> λ[α => String])(
    decorateReferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration(s"<def ${showRef(ref)}>", "</def>")),
    decorateUnreferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration("", "")),
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit E: HEqualK[Ptr]): Show[A] = new Show[A] {
    override def shows(a: A): String =
      DeepShow.this.show(a).showAutoLabeled(deref, showRef)(decorateReferenced, decorateUnreferenced, decorateReference)
  }

  def shallow(implicit S: ShowK[Ptr]): Show[A] = new Show[A] {
    override def shows(a: A): String = DeepShow.this.show(a).showShallow(S)
  }
}

object DeepShow {

  trait FromShow[A, Ptr[_]] extends DeepShow[A, Ptr] {
    def write[O](out: O, a: A)(implicit ev: ObjectOutput[O, String, Ptr]): O =
      show(a).writeTo(out)

    def serialize[M[_]](a: A)(implicit ev: MonadObjectOutput[M, String, Ptr], M1: BindRec[M]): M[Unit] =
      show(a).serialize[M]
  }

  trait FromWrite[A, Ptr[_]] extends DeepShow[A, Ptr] with ObjectSerializer.FromWrite[A, String, Ptr] {
    def show(a: A): Desc[Ptr] = write(Desc.empty[Ptr], a)
  }

  trait FromSerialize[A, Ptr[_]] extends DeepShow[A, Ptr] with ObjectSerializer.FromSerialize[A, String, Ptr] {
    def show(a: A): Desc[Ptr] = serialize[FreeObjectOutput[String, Ptr, ?]](a)
  }

  def toShow[Ptr[_], A](deref: Ptr ~> Id)(implicit ev: DeepShow[A, Ptr], S: ShowK[Ptr], E: HEqualK[Ptr]): Show[A] =
    ev.toShow(deref, S)()

  implicit def specialize[A[_[_]], Ptr[_]](implicit ev: DeepShowK[A]): DeepShow[A[Ptr], Ptr] =
    ev.specialize[Ptr]
}

trait DeepShowK[A[_[_]]] {
  def show[Ptr[_]](a: A[Ptr]): Desc[Ptr]

  def specialize[Ptr[_]]: DeepShow[A[Ptr], Ptr] = new DeepShow.FromShow[A[Ptr], Ptr] {
    def show(a: A[Ptr]): Desc[Ptr] = DeepShowK.this.show(a)
  }
}



