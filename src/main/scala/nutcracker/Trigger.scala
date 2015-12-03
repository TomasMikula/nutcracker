package nutcracker

import scala.language.higherKinds

sealed trait Trigger[K[_]]
case class Discard[K[_]]() extends Trigger[K]
case class Sleep[K[_]]() extends Trigger[K]
case class Fire[K[_]](cont: K[Unit]) extends Trigger[K]
case class FireReload[K[_]](cont: K[Unit]) extends Trigger[K]