package nutcracker

/**
  *
  * @tparam U description of unresolved things
  */
sealed trait Assessment[+U] {
  import Assessment._

  /** When this assessment is [[Done]] or [[Stuck]], use `that`. */
  def orElse[V >: U](that: => Assessment[V]): Assessment[V] = this match {
    case Done => that
    case Stuck => that
    case a => a
  }
}

object Assessment {
  case object Failed extends Assessment[Nothing]
  case object Done extends Assessment[Nothing]
  case object Stuck extends Assessment[Nothing]
  case class Incomplete[U](unresolved: U) extends Assessment[U]
}