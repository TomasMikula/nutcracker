package nutcracker.demo.simpletypeinference.types

import nutcracker.demo.simpletypeinference.kinds._

sealed trait Routing[K, L](using
  val inKind: Kind[K],
  val outKind: Kind[L],
) {
  import Routing._

  def applyTo[TA[_], J](args: ArgIntro[TA, J, K]): ApplyRes[TA, J, ?, L] = {
    import args.inKind

    this match {
      case Id() => ApplyRes(Id(), args)
    }
  }
}

object Routing {
  case class Id[K: Kind]() extends Routing[K, K]

  case class ApplyRes[TA[_], K, X, L](r: Routing[K, X], ai: ArgIntro[TA, X, L])

  def id[K: Kind]: Routing[K, K] =
    Id()

  def proveId[K](r: Routing[○, K]): K =:= ○ =
    r match {
      case Id() => implicitly[K =:= ○]
    }
}
