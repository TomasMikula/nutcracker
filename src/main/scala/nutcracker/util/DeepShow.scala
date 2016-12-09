package nutcracker.util

import scalaz.Free.Trampoline
import scalaz.{Trampoline, ~>}
import scalaz.Id._

/** Printing of (potentially cyclic) object graphs.
  * Features:
  *  - abstracted over pointers;
  *  - termination and correctness in presence of cycles;
  *  - stack safety.
  */
trait DeepShow[A, Ptr[_]] {
  import DeepShow._

  def show(a: A): Desc[Ptr]

  final def deepShow(a: A)(deref: Ptr ~> Id)(
    decorateReferenced: (=> String) => (String, String) = ref => (s"<def $ref>", "</def>"),
    decorateUnreferenced: (=> String) => (String, String) = ref => ("", ""),
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit S: ShowK[Ptr], E: HEqualK[Ptr]): String =
    DeepShow.deepShow(show(a), deref)(decorateReferenced, decorateUnreferenced, decorateReference)
}

object DeepShow {
  sealed trait Desc[Ptr[_]] {
    def +(that: Desc[Ptr]): Desc[Ptr] = Concat(this, that)
  }
  private final case class Referenced[Ptr[_], A](pa: Ptr[A], ev: DeepShow[A, Ptr]) extends Desc[Ptr]
  private final case class RefString[Ptr[_], A](p: Ptr[A]) extends Desc[Ptr]
  private final case class Write[Ptr[_]](s: String) extends Desc[Ptr]
  private final case class Concat[Ptr[_]](l: Desc[Ptr], r: Desc[Ptr]) extends Desc[Ptr]

  object Desc {
    def apply[Ptr[_], A](a: A)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
      ev.show(a)

    def ref[Ptr[_], A](pa: Ptr[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
      Referenced(pa, ev)

    def refString[Ptr[_], A](pa: Ptr[A]): Desc[Ptr] =
      RefString(pa)

    def done[Ptr[_]](s: String): Desc[Ptr] =
      Write(s)
  }

  def deepShow[Ptr[_], A](a: A)(deref: Ptr ~> Id)(implicit ev: DeepShow[A, Ptr], S: ShowK[Ptr], E: HEqualK[Ptr]): String =
    ev.deepShow(a)(deref)()

  private[DeepShow] def deepShow[Ptr[_]](fa: Desc[Ptr], deref: Ptr ~> Id)(
    decorateReferenced: (=> String) => (String, String),
    decorateUnreferenced: (=> String) => (String, String),
    decorateReference: String => String
  )(implicit S: ShowK[Ptr], E: HEqualK[Ptr]): String = {
    type Γ = List[Ptr[_]] // context, i.e. all parents
    type R = Lst[Ptr[_]]  // referenced within the subgraph

    def go(node: Desc[Ptr], visited: Γ): Trampoline[(Lst[String], R)] = node match {
      case Write(s) => Trampoline.done((Lst.singleton(s), Lst.empty))
      case RefString(pa) => Trampoline.done((Lst.singleton(S.shows(pa)), Lst.empty))
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

    val (res, ref) = go(fa, Nil).run

    assert(ref.isEmpty)

    res.foldLeft(new StringBuilder)((acc, s) => acc.append(s)).toString
  }

  def setDesc[Ptr[_], A](sa: Set[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] =
    Desc.done("{") + mkString(sa)(", ") + Desc.done("}")

  def mkString[Ptr[_], A](sa: Iterable[A])(sep: String)(implicit ev: DeepShow[A, Ptr]): Desc[Ptr] = {
    val it = sa.iterator
    if (it.hasNext) {
      val h = it.next()
      it.foldLeft(ev.show(h))((acc, a) => acc + Desc.done(sep) + ev.show(a))
    } else
      Desc.done("")
  }

  implicit def specialize[A[_[_]], Ptr[_]](implicit ev: DeepShowK[A]): DeepShow[A[Ptr], Ptr] =
    ev.specialize[Ptr]
}

trait DeepShowK[A[_[_]]] {
  import nutcracker.util.DeepShow.Desc

  def show[Ptr[_]](a: A[Ptr]): Desc[Ptr]

  def specialize[Ptr[_]]: DeepShow[A[Ptr], Ptr] = new DeepShow[A[Ptr], Ptr] {
    def show(a: A[Ptr]): Desc[Ptr] = DeepShowK.this.show(a)
  }
}