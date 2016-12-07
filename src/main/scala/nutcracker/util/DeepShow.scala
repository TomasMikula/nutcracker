package nutcracker.util

import nutcracker.util.DeepShow.Desc

import scalaz.Free.Trampoline
import scalaz.{Free, Trampoline, WriterT, ~>}
import scalaz.Id._

/** Printing of (potentially cyclic) object graphs.
  * Features:
  *  - abstracted over pointers;
  *  - termination and correctness in presence of cycles;
  *  - stack safety.
  */
trait DeepShow[A, Ptr[_]] {
  def show(a: A): Free[Desc[Ptr, ?], String]

  final def deepShow(a: A)(deref: Ptr ~> Id)(
    decorateReferenced: (String, => String) => String = (str, ref) => s"<def $ref>$str</def>",
    decorateUnreferenced: (String, => String) => String = (str, ref) => str,
    decorateReference: String => String = ref => s"<ref $ref/>"
  )(implicit S: ShowK[Ptr], E: HEqualK[Ptr]): String =
    DeepShow.deepShow(show(a), deref)(decorateReferenced, decorateUnreferenced, decorateReference)
}

object DeepShow {
  sealed trait Desc[Ptr[_], A]
  private final case class Referenced[Ptr[_], A](pa: Ptr[A], ev: DeepShow[A, Ptr]) extends Desc[Ptr, String]

  object Desc {
    def apply[Ptr[_], A](pa: Ptr[A])(implicit ev: DeepShow[A, Ptr]): Desc[Ptr, String] =
      Referenced(pa, ev)
  }

  private[DeepShow] def deepShow[Ptr[_]](fa: Free[Desc[Ptr, ?], String], deref: Ptr ~> Id)(
    decorateReferenced: (String, => String) => String,
    decorateUnreferenced: (String, => String) => String,
    decorateReference: String => String
  )(implicit S: ShowK[Ptr], E: HEqualK[Ptr]): String = {
    type Γ = List[Ptr[_]]

    import scalaz.std.list._

    def interpret(visited: Γ): Desc[Ptr, ?] ~> WriterT[Trampoline, Γ, ?] =
      λ[Desc[Ptr, ?] ~> WriterT[Trampoline, Γ, ?]](_ match {
        case Referenced(pa, ev) =>
          if(visited.exists(E.hEqual(_, pa))) WriterT(Trampoline.done((List(pa), decorateReference(S.shows(pa)))))
          else WriterT(Trampoline.suspend({
            ev.show(deref(pa)).foldMap(interpret(pa :: visited)).run map {
              case (referenced, str) =>
                val referenced1 = referenced.filterNot(E.hEqual(_, pa))
                if(referenced1.size < referenced.size) (referenced1, decorateReferenced(str, S.shows(pa)))
                else (referenced1, decorateUnreferenced(str, S.shows(pa)))
            }
          }))
      })

    fa.foldMap(interpret(List())).run.run._2
  }
}

