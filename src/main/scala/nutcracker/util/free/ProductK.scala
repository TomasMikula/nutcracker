package nutcracker.util.free

import scala.language.higherKinds

final case class ProductK[F[_[_]], G[_[_]], K[_]](run: (F[K], G[K])) extends AnyVal {
  def _1 = run._1
  def _2 = run._2
  def update_1(f: F[K]) = ProductK[F, G, K]((f, run._2))
  def update_2(g: G[K]) = ProductK[F, G, K]((run._1, g))
}
