package nutcracker

sealed trait Trigger
case object Discard extends Trigger
case object Sleep extends Trigger
case class Fire(cont: ProblemDescription[Unit]) extends Trigger
case class FireReload(cont: ProblemDescription[Unit]) extends Trigger