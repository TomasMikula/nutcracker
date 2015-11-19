package nutcracker

sealed trait CellRef[D]
case class PureDomRef[+A, D] private[nutcracker](private[nutcracker] val domainId: Long) extends CellRef[D]