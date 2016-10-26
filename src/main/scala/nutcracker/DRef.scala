package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions
import nutcracker.PropagationLang._
import nutcracker.util.{ContU, Inject}

import scalaz.{Bind, Equal, Show}
import scalaz.syntax.bind._

sealed abstract class DRef[D](private[nutcracker] val domainId: Long) {
  type Update
  type Delta

  /** Infer `Update` and `Delta` types. Relies on global uniqueness
    * of `Dom[D]` instances.
    */
  def infer(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
    this.asInstanceOf[DRef.Aux[D, dom.Update, dom.Delta]]
}

object DRef {
  type Aux[D, U, Δ] = DRef[D] { type Update = U; type Delta = Δ }

  def apply[D](domainId: Long)(implicit dom: Dom[D]): DRef.Aux[D, dom.Update, dom.Delta] =
    new DRef[D](domainId) {
      type Update = dom.Update
      type Delta = dom.Delta
    }

  implicit def drefOps[D](ref: DRef[D]) = DRefOps[D, ref.Update, ref.Delta](ref)

  final case class DRefOps[D, U, Δ](ref: DRef.Aux[D, U, Δ]) extends AnyVal {

    def ==>[M[_]: Propagation](target: DRef.Aux[D, U, Δ])(implicit inj: Inject[Join[D], U], dom: Dom.Aux[D, U, Δ]): M[Unit] =
      Propagation[M].valTrigger(ref){ d => FireReload(FinalVars[M].join(target)(d)) }

    def <=>[M[_]: Propagation: Bind](target: DRef.Aux[D, U, Δ])(implicit inj: Inject[Join[D], U], dom: Dom.Aux[D, U, Δ]): M[Unit] =
      (ref ==> target) >> (target ==> ref)

    def >>=[M[_]: Propagation, A](f: A => M[Unit])(implicit fin: Final.Aux[D, A], dom: Dom[D]): M[Unit] =
      FinalVars[M].whenFinal(ref).exec(f)

    def asCont[M[_]: Propagation](implicit fin: Final[D]): ContU[M, fin.Out] =
      ContU { FinalVars[M].whenFinal(ref).exec(_) }

    def peekC[M[_]](implicit M: Propagation[M]): ContU[M, D] =
      ContU(f => M.peek(ref)(f))

  }

  implicit def toCont[M[_], D](ref: DRef[D])(implicit M: Propagation[M], fin: Final[D]): ContU[M, fin.Out] =
    ref.asCont

  implicit def equalInstance[D, U, Δ]: Equal[DRef.Aux[D, U, Δ]] = new Equal[DRef.Aux[D, U, Δ]] {
    def equal(r1: Aux[D, U, Δ], r2: Aux[D, U, Δ]): Boolean = r1.domainId == r2.domainId
  }

  implicit def showInstance[D, U, Δ]: Show[DRef.Aux[D, U, Δ]] = new Show[DRef.Aux[D, U, Δ]] {
    override def shows(ref: DRef.Aux[D, U, Δ]): String = s"ref${ref.domainId}"
  }
}
