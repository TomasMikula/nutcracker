package nutcracker.util

import nutcracker.util.FreeObjectOutput.Decoration

import scalaz.Id.Id
import scalaz.{BindRec, Semigroup, ~>}
import scalaz.syntax.monad._

final class Desc[Ptr[_]] private(private[util] val unwrap: FreeObjectOutput[String, Ptr, Unit]) extends AnyVal {
  import Desc._

  def ++(that: Desc[Ptr]): Desc[Ptr] = wrap(this.unwrap >> that.unwrap)
  def :+(s: String): Desc[Ptr] = this ++ wrap(FreeObjectOutput.write(s))
  def +:(s: String): Desc[Ptr] = wrap[Ptr](FreeObjectOutput.write(s)) ++ this

  def writeTo[O](out: O)(implicit O: ObjectOutput[O, String, Ptr]): O =
    unwrap.writeTo(out)

  def serialize[M[_]](implicit M: MonadObjectOutput[M, String, Ptr], M1: BindRec[M]): M[Unit] =
    unwrap.serialize[M]

  def eval(deref: Ptr ~> Id, showRef: Ptr ~> λ[α => String])(
    decorateReferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration(s"<def ${showRef(ref)}>", "</def>")),
    decorateUnreferenced: Ptr ~> λ[α => Decoration[String]] = λ[Ptr ~> λ[α => Decoration[String]]](ref => Decoration("", "")),
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit E: HEqualK[Ptr]): String =
    unwrap.toString(
      deref,
      decorateReferenced,
      decorateUnreferenced,
      λ[Ptr ~> λ[α => String]](p => decorateReference(showRef(p)))
    )

  def shallowEval(implicit S: ShowK[Ptr]): String =
    unwrap.toString(S)
}

object Desc {
  private[util] def wrap[Ptr[_]](f: FreeObjectOutput[String, Ptr, Unit]): Desc[Ptr] = new Desc(f)

  def apply[Ptr[_], A](a: A)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    ev.show(a)

  def ref[Ptr[_], A](pa: Ptr[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    wrap(FreeObjectOutput.writeObject(pa, ev))

  def done[Ptr[_]](s: String): Desc[Ptr] =
    wrap(FreeObjectOutput.write(s))

  def empty[Ptr[_]]: Desc[Ptr] =
    wrap(FreeObjectOutput.empty[String, Ptr])

  def setDesc[Ptr[_], A](sa: Set[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    Desc.done("{") ++ mkString(sa)(", ") ++ Desc.done("}")

  def mkString[Ptr[_], A](sa: Iterable[A])(sep: String)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] = {
    val it = sa.iterator.map(ev.show)
    join(it)(sep)
  }

  def join[Ptr[_]](descs: Iterable[Desc[Ptr]])(sep: String): Desc[Ptr] = {
    join(descs.iterator)(sep)
  }

  private def join[Ptr[_]](it: Iterator[Desc[Ptr]])(sep: String): Desc[Ptr] = {
    if (it.hasNext) {
      val h = it.next()
      it.foldLeft(h)((acc, d) => acc ++ Desc.done(sep) ++ d)
    } else
      Desc.done("")
  }

  implicit def descSemigroup[Ptr[_]]: Semigroup[Desc[Ptr]] = new Semigroup[Desc[Ptr]] {
    def append(f1: Desc[Ptr], f2: => Desc[Ptr]): Desc[Ptr] = f1 ++ f2
  }

  implicit def stringAggregator[Ptr[_]]: Aggregator[Desc[Ptr], String] =
    Aggregator(_ :+ _)
}