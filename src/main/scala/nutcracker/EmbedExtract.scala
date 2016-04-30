package nutcracker

trait EmbedExtract[A, D] {
  def embed(a: A): D

  /** Must be consistend with [[Dom.assess()]]:
    * returns `Some` iff [[Dom.assess()]] returns [[Dom.Refined]].
    */
  def extract(d: D): Option[A]
}

object EmbedExtract {
  def apply[A, D](implicit ee: EmbedExtract[A, D]): EmbedExtract[A, D] = ee
}