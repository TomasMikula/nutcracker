package nutcracker.util.free

import scala.language.higherKinds
import scalaz.Monoid

final case class ProductK[F[_[_]], G[_[_]], K[_]](_1: F[K], _2: G[K]) {
  def update_1(f: F[K]) = ProductK[F, G, K](f, _2)
  def update_2(g: G[K]) = ProductK[F, G, K](_1, g)
  def :*:[E[_[_]]](e: E[K]): ProductK[E, ProductK[F, G, ?[_]], K] = ProductK[E, ProductK[F, G, ?[_]], K](e, this)
}

object ProductK {
  implicit class ProductKOps[G[_[_]], K[_]](g: G[K]) {
    def :*:[F[_[_]]](f: F[K]): ProductK[F, G, K] = ProductK(f, g)
  }

  implicit def monoidK[F[_[_]], G[_[_]]](implicit F: MonoidK[F], G: MonoidK[G]): MonoidK[ProductK[F, G, ?[_]]] = new MonoidK[ProductK[F, G, ?[_]]] {
    def zero[K[_]]: ProductK[F, G, K] = ProductK(F.zero, G.zero)

    def append[K[_]](p1: ProductK[F, G, K], p2: ProductK[F, G, K]): ProductK[F, G, K] =
      ProductK(F.append(p1._1, p2._1), G.append(p1._2, p2._2))
  }

  implicit def monoid[F[_[_]], G[_[_]], K[_]](implicit F: Monoid[F[K]], G: Monoid[G[K]]): Monoid[ProductK[F, G, K]] = new Monoid[ProductK[F, G, K]] {
    def zero: ProductK[F, G, K] = ProductK(F.zero, G.zero)

    def append(p1: ProductK[F, G, K], p2: => ProductK[F, G, K]): ProductK[F, G, K] =
      ProductK(F.append(p1._1, p2._1), G.append(p1._2, p2._2))
  }
}