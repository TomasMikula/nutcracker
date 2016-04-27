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

  implicit def setInstance[A]: EmbedExtract[A, Set[A]] = new EmbedExtract[A, Set[A]] {
    def embed(a: A): Set[A] = Set(a)
    def extract(d: Set[A]): Option[A] = if(d.size == 1) Some(d.head) else None
  }
}