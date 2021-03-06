package nutcracker.data

import nutcracker.JoinDom

object intMax {

  /** Max semilattice for `Int` */
  implicit val intDom: JoinDom[Int] = new JoinDom.Template[Int] {
    def ljoin0(d1: Int, d2: Int): Option[Int] = if(d2 > d1) Some(d2) else None
    def isFailed(d: Int): Boolean = false
  }
}