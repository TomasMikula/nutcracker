package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.{FreeK, Inject, InjectK}
import scalaz.Cont

sealed abstract class DRef[D](private[nutcracker] val domainId: Long) {
  type Update
  type Delta

  /** Infer `Update` and `Delta` types. Relies on global uniqueness
    * `Dom[D]` instance.
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

    def ==>(target: DRef.Aux[D, U, Δ])(implicit inj: Inject[Meet[D], U], dom: Dom.Aux[D, U, Δ]): FreeK[PropagationLang, Unit] =
      valTriggerF(ref){ d => fireReload(meet(target)(d)) }

    def <=>(target: DRef.Aux[D, U, Δ])(implicit inj: Inject[Meet[D], U], dom: Dom.Aux[D, U, Δ]): FreeK[PropagationLang, Unit] =
      (ref ==> target) >> (target ==> ref)

    def >>=[F[_[_], _], A](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], ex: Extract.Aux[D, A], dom: Dom[D]): FreeK[F, Unit] =
      whenResolved(ref).exec(f)

    def asCont[F[_[_], _]](implicit inj: InjectK[PropagationLang, F], ex: Extract[D]): Cont[FreeK[F, Unit], ex.Out] =
      Cont { whenResolved(ref).exec(_) }

  }

  implicit def toCont[F[_[_], _], D](ref: DRef[D])(implicit inj: InjectK[PropagationLang, F], ex: Extract[D]): Cont[FreeK[F, Unit], ex.Out] =
    ref.asCont
}
