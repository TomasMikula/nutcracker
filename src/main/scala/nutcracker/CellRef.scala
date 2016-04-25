package nutcracker

import scala.language.higherKinds
import scala.language.implicitConversions

import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.{InjectK, FreeK}

import scalaz.Cont

sealed trait CellRef[D]

case class DomRef[A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D] {

  def ==>(target: DomRef[_, D]): FreeK[PropagationLang, Unit] = varTriggerF(this){ d => fireReload(intersectF(target)(d)) }
  def <=>(target: DomRef[_, D]): FreeK[PropagationLang, Unit] = (this ==> target) >> (target ==> this)
  def >>=[F[_[_], _]](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): FreeK[F, Unit] = whenResolvedF(this)(f)

  def asCont[F[_[_], _]](implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): Cont[FreeK[F, Unit], A] =
    Cont { whenResolvedF(DomRef.this)(_) }
}

object DomRef {

  implicit def toCont[F[_[_], _], A, D](d: DomRef[A, D])(implicit inj: InjectK[PropagationLang, F], dom: Domain[A, D]): Cont[FreeK[F, Unit], A] =
    d.asCont

}