package nutcracker

trait Extract[D, A] {
  /** Must be consistent with [[Dom.assess()]]:
    * returns `Some` iff [[Dom.assess()]] returns [[Dom.Refined]].
    */
  def extract(d: D): Option[A]
}

object Extract {
  def apply[D, A](implicit ex: Extract[D, A]): Extract[D, A] = ex
}