package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions

import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.{InjectK, FreeK}

import scalaz.Cont

sealed trait VRef[D] {
  def asCont[F[_[_], _], A](implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): Cont[FreeK[F, Unit], A] =
    Cont { whenResolvedF(VRef.this)(_) }

}

object VRef {
  implicit def toCont[F[_[_], _], A, D](ref: VRef[D])(implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): Cont[FreeK[F, Unit], A] =
    ref.asCont
}

sealed case class DRef[D, U, Δ](domainId: Long) extends VRef[D] {

  def ==>(target: LRef[D]): FreeK[PropagationLang, Unit] = varTriggerF(this){ d => fireReload(intersectF(target)(d)) }
  def <=>(target: LRef[D])(implicit ev: this.type <:< LRef[D]): FreeK[PropagationLang, Unit] = (this ==> target) >> (target ==> ev(this))
  def >>=[F[_[_], _], A](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): FreeK[F, Unit] = whenResolvedF(this)(f)

}
