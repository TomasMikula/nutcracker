package nutcracker

trait Embed[A, D] {
  def embed(a: A): D
}

object Embed {
  def apply[A, D](implicit em: Embed[A, D]): Embed[A, D] = em
}