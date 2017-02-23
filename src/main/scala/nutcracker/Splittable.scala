package nutcracker

trait Splittable[D] extends Dom[D] {
  import Splittable._

  def assess(d: D): Status[Update]

  def isFailed(d: D): Boolean = assess(d) match {
    case Failed => true
    case _ => false
  }

  def isUnresolved(d: D): Boolean = assess(d) match {
    case Unrefined(_) => true
    case _ => false
  }
}

object Splittable {
  type Aux[D, U, Δ] = Splittable[D] { type Update = U; type Delta = Δ }

  sealed trait Status[+U]
  case class Unrefined[U](xor: () => Option[List[U]]) extends Status[U]
  case object Refined extends Status[Nothing]
  case object Failed extends Status[Nothing]

  implicit class SplittableOps[D, U, Δ](d: D)(val dom: Splittable.Aux[D, U, Δ]) {
    def assess: Status[U] = dom.assess(d)
  }
}