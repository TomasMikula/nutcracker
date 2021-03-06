package nutcracker

/** Typeclass for domains that have (some) "_final_" elements, i.e. elements
  * that cannot be refined further without reaching a contradiction. In other
  * words, typeclass for domains that have elements _covered_ by top.
  */
trait Final[D] {
  /** For many domains, _final_ elements (i.e. elements _covered_ by top)
    * can be given a more precise type than `D`. For example, final elements
    * of `Promise[A]` correspond to elements of `A`; final elements of integer
    * interval sets correspond to integers; etc. This type member represents
    * the type of final elements of `D`.
    */
  type Out

  /** Returns `Some` if `d` is _final_ (i.e. _covered_ by top),
    * `None` otherwise.
    */
  def extract(d: D): Option[Out]

  /** The reverse of [[extract]].
    */
  def embed(a: Out): D

  def isFinal(d: D): Boolean = extract(d).isDefined
}

object Final {
  type Aux[D, A] = Final[D] { type Out = A }

  def apply[D](implicit fin: Final[D]): Final.Aux[D, fin.Out] = fin
}