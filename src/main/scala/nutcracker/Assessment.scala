package nutcracker

/**
  *
  * @tparam U description of unresolved things
  */
sealed trait Assessment[+U]

object Assessment {
  case object Failed extends Assessment[Nothing]
  case object Done extends Assessment[Nothing]
  case class Incomplete[U](unresolved: U) extends Assessment[U]
}