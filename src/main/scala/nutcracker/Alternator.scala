package nutcracker

sealed trait Alternator

object Alternator {
  case object Left extends Alternator
  case object Right extends Alternator
  case object Stop extends Alternator
}