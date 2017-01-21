package nutcracker.util

import scalaz.Free.Trampoline
import scalaz.{Semigroup, Trampoline, ~>}
import scalaz.Id._

/** Printing of (potentially cyclic) object graphs.
  * Features:
  *  - abstracted over pointers;
  *  - termination and correctness in presence of cycles;
  *  - stack safety.
  */
trait DeepShow[A, Ptr[_]] {
  def show(a: A): Desc[Ptr]

  final def deepShow(a: A)(deref: Ptr ~> Id)(
    decorateReferenced: (=> String) => (String, String) = ref => (s"<def $ref>", "</def>"),
    decorateUnreferenced: (=> String) => (String, String) = ref => ("", ""),
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit S: ShowK[Ptr], E: HEqualK[Ptr]): String =
    show(a).eval(deref)(decorateReferenced, decorateUnreferenced, decorateReference)
}

object DeepShow {

  def deepShow[Ptr[_], A](a: A)(deref: Ptr ~> Id)(implicit ev: DeepShow[A, Ptr], S: ShowK[Ptr], E: HEqualK[Ptr]): String =
    ev.deepShow(a)(deref)()

  implicit def specialize[A[_[_]], Ptr[_]](implicit ev: DeepShowK[A]): DeepShow[A[Ptr], Ptr] =
    ev.specialize[Ptr]
}

trait DeepShowK[A[_[_]]] {
  def show[Ptr[_]](a: A[Ptr]): Desc[Ptr]

  def specialize[Ptr[_]]: DeepShow[A[Ptr], Ptr] = new DeepShow[A[Ptr], Ptr] {
    def show(a: A[Ptr]): Desc[Ptr] = DeepShowK.this.show(a)
  }
}

sealed trait Desc[Ptr[_]] {
  import Desc._

  def ++(that: Desc[Ptr]): Desc[Ptr] = Concat(this, that)
  def :+(s: String): Desc[Ptr] = Concat(this, Write(s))
  def +:(s: String): Desc[Ptr] = Concat(Write(s), this)

  def eval(deref: Ptr ~> Id)(
    decorateReferenced: (=> String) => (String, String),
    decorateUnreferenced: (=> String) => (String, String),
    decorateReference: String => String
  )(implicit S: ShowK[Ptr], E: HEqualK[Ptr]): String = {
    type Γ = List[Ptr[_]] // context, i.e. all parents
    type R = Lst[Ptr[_]]  // referenced within the subgraph

    def go(node: Desc[Ptr], visited: Γ): Trampoline[(Lst[String], R)] = node match {
      case Write(s) => Trampoline.done((Lst.singleton(s), Lst.empty))
      case Referenced(pa, ev) =>
        if(visited.exists(E.hEqual(_, pa))) Trampoline.done((Lst.singleton(decorateReference(S.shows(pa))), Lst.singleton(pa)))
        else Trampoline.suspend(go(ev.show(deref(pa)), pa :: visited)) map {
          case (res, referenced) =>
            val referenced1 = referenced.filterNot(E.hEqual(_, pa))
            val (pre, post) = if(referenced1.size < referenced.size) decorateReferenced(S.shows(pa)) else decorateUnreferenced(S.shows(pa))
            (pre +: res :+ post, referenced1)
        }
      case Concat(l, r) =>
        Trampoline.suspend(go(l, visited)) flatMap { case (res1, ref1) =>
          go(r, visited) map { case (res2, ref2) => (res1 ++ res2, ref1 ++ ref2) }
        }
    }

    val (res, ref) = go(this, Nil).run

    assert(ref.isEmpty)

    res.foldLeft(new StringBuilder)((acc, s) => acc.append(s)).toString
  }
}

object Desc {
  private[Desc] final case class Referenced[Ptr[_], A](pa: Ptr[A], ev: DeepShow[A, Ptr]) extends Desc[Ptr]
  private[Desc] final case class Write[Ptr[_]](s: String) extends Desc[Ptr]
  private[Desc] final case class Concat[Ptr[_]](l: Desc[Ptr], r: Desc[Ptr]) extends Desc[Ptr]

  def apply[Ptr[_], A](a: A)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    ev.show(a)

  def ref[Ptr[_], A](pa: Ptr[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    Referenced(pa, ev)

  def done[Ptr[_]](s: String): Desc[Ptr] =
    Write(s)

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