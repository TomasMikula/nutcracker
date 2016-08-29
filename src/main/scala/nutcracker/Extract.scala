package nutcracker

/** Typeclass for domains that have (some) "_final_" elements, i.e. elements
  * that cannot be refined further without reaching a contradiction. In other
  * words, typeclass for domains that have elements _covered_ by top. Final
  * elements must be considered _refined_ by [[Dom.assess()]].
  */
trait Extract[D] {
  /** For many domains, _final_ elements (i.e. elements _covered_ by top)
    * can be given a more precise type than `D`. For example, final elements
    * of `Promise[A]` correspond to elements of `A`; final elements of integer
    * interval sets correspond to integers; etc. This type member represents
    * the type of final elements of `D`.
    */
  type Out

  /** Returns `Some` if `d` is _final_ (i.e. _covered_ by top),
    * `None` otherwise.
    * Must be consistent with [[Dom.assess()]]: If this method returns `Some`,
    * [[Dom.assess()]] must return [[Dom.Refined]].
    */
  def extract(d: D): Option[Out]
}

object Extract {
  type Aux[D, A] = Extract[D] { type Out = A }

  def apply[D](implicit ex: Extract[D]): Extract.Aux[D, ex.Out] = ex
}