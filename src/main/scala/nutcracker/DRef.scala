package nutcracker

import nutcracker.Dom.Meet

import scala.language.higherKinds
import scala.language.implicitConversions
import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.{FreeK, Inject, InjectK}

import scalaz.Cont

sealed trait VRef[D]

sealed case class DRef[D, U, Δ](domainId: Long) extends VRef[D] {

  def ==>[U1](target: DRef[D, U1, _])(implicit inj: Inject[Meet[D], U1]): FreeK[PropagationLang, Unit] =
    valTriggerF(this){ d => fireReload(intersectF(target)(d)) }
  def <=>(target: DRef[D, U, _])(implicit inj: Inject[Meet[D], U]): FreeK[PropagationLang, Unit] =
    (this ==> target) >> (target ==> this)
  def >>=[F[_[_], _], A](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): FreeK[F, Unit] =
    whenResolvedF(this)(f)

  def asCont[F[_[_], _], A](implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): Cont[FreeK[F, Unit], A] =
    Cont { whenResolvedF(DRef.this)(_) }
}

object DRef {
  implicit def toCont[F[_[_], _], A, D](ref: DRef[D, _, _])(implicit inj: InjectK[PropagationLang, F], ee: EmbedExtract[A, D]): Cont[FreeK[F, Unit], A] =
    ref.asCont
}
