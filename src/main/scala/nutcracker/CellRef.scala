package nutcracker

import nutcracker.ProblemDescription.{Second, Intersect, varTrigger}
import nutcracker.Triggers.FireReload

sealed trait CellRef[D]
case class PureDomRef[+A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D] {
  def ==>(target: PureDomRef[_, D]): ProblemDescription[Unit] = varTrigger(this){ d => FireReload(Intersect(target, d)) }
  def <=>(target: PureDomRef[_, D]): ProblemDescription[Unit] = Second(this ==> target, target ==> this)
}