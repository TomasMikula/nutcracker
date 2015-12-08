package nutcracker

import scala.language.higherKinds

import nutcracker.PropagationLang._
import nutcracker.Trigger._
import nutcracker.util.free.{InjectK, FreeK}

sealed trait CellRef[D]
case class DomRef[+A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D] {
  def ==>(target: DomRef[_, D]): FreeK[PropagationLang, Unit] = varTriggerF(this){ d => fireReload(intersectF(target)(d)) }
  def <=>(target: DomRef[_, D]): FreeK[PropagationLang, Unit] = (this ==> target) >> (target ==> this)
  def >>=[F[_[_], _]](f: A => FreeK[F, Unit])(implicit inj: InjectK[PropagationLang, F]): FreeK[F, Unit] = whenResolvedF(this)(f)
}