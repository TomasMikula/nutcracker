package nutcracker

import nutcracker.ProblemDescription._

sealed trait CellRef[D]
case class DomRef[+A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D] {
  def ==>(target: DomRef[_, D]): ProblemDescription[Unit] = varTrigger(this){ d => FireReload(Intersect(target, d)) }
  def <=>(target: DomRef[_, D]): ProblemDescription[Unit] = Second(this ==> target, target ==> this)
  def >>=[B](f: A => ProblemDescription[B]): ProblemDescription[B] = fetchResult(this).flatMap(f)
}