package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

sealed trait Routing[K, L](using val inKind: Kind[K], val outKind: Kind[L])
object Routing {
  case class Id[K: Kind]() extends Routing[K, K]

  def id[K: Kind]: Routing[K, K] =
    Id()

  def proveId[K](r: Routing[○, K]): K =:= ○ =
    r match {
      case Id() => implicitly[K =:= ○]
    }
}
