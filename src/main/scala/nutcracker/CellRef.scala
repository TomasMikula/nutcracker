package nutcracker

import nutcracker.ProblemDescription.{Second, Intersect, VarTrigger}

sealed trait CellRef[D]
case class PureDomRef[+A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D] {
  def ==>(target: PureDomRef[_, D]): ProblemDescription[Unit] = VarTrigger(this, (d: D) => Intersect(target, d))
  def <=>(target: PureDomRef[_, D]): ProblemDescription[Unit] = Second(this ==> target, target ==> this)
}