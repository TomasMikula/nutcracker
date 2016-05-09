package nutcracker

trait Extract[D] {
  type Out

  /** Must be consistent with [[Dom.assess()]]:
    * returns `Some` iff [[Dom.assess()]] returns [[Dom.Refined]].
    */
  def extract(d: D): Option[Out]
}

object Extract {
  type Aux[D, A] = Extract[D] { type Out = A }

  def apply[D](implicit ex: Extract[D]): Extract.Aux[D, ex.Out] = ex
}