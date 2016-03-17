package nutcracker.util.free

import monocle.Lens
import nutcracker.util.ValK

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

  implicit def leftLens[F[_[_]], G[_[_]], K[_]]: Lens[ProductK[F, G, K], F[K]] =
    Lens[ProductK[F, G, K], F[K]](_._1)(fk => pk => pk.update_1(fk))

  implicit def leftLensZ[F[_[_]], G[_[_]], K[_]]: scalaz.Lens[ProductK[F, G, K], F[K]] =
    scalaz.Lens[ProductK[F, G, K], F[K]](pk => scalaz.Store(fk => pk.update_1(fk), pk._1))

  implicit def leftLensZK[F[_[_]], G[_[_]]]: ValK[λ[K[_] => scalaz.Lens[ProductK[F, G, K], F[K]]]] =
    new ValK[λ[K[_] => scalaz.Lens[ProductK[F, G, K], F[K]]]] {
      def compute[K[_]]: scalaz.Lens[ProductK[F, G, K], F[K]] =
        scalaz.Lens[ProductK[F, G, K], F[K]] (pk => scalaz.Store (fk => pk.update_1 (fk), pk._1) )
    }

  implicit def rightLens[F[_[_]], G[_[_]], K[_]]: Lens[ProductK[F, G, K], G[K]] =
    Lens[ProductK[F, G, K], G[K]](_._2)(gk => pk => pk.update_2(gk))

  implicit def rightLensZ[F[_[_]], G[_[_]], K[_]]: scalaz.Lens[ProductK[F, G, K], G[K]] =
    scalaz.Lens[ProductK[F, G, K], G[K]](pk => scalaz.Store(gk => pk.update_2(gk), pk._2))

  implicit def rightLensZK[F[_[_]], G[_[_]]]: ValK[λ[K[_] => scalaz.Lens[ProductK[F, G, K], G[K]]]] =
    new ValK[λ[K[_] => scalaz.Lens[ProductK[F, G, K], G[K]]]] {
      def compute[K[_]]: scalaz.Lens[ProductK[F, G, K], G[K]] =
        scalaz.Lens[ProductK[F, G, K], G[K]](pk => scalaz.Store(gk => pk.update_2(gk), pk._2))
    }

  implicit def leftComposedLens[F[_[_]], G[_[_]], K[_], A](implicit lfa: Lens[F[K], A]): Lens[ProductK[F, G, K], A] =
    leftLens[F, G, K].composeLens(lfa)

  implicit def rightComposedLens[F[_[_]], G[_[_]], K[_], A](implicit lga: Lens[G[K], A]): Lens[ProductK[F, G, K], A] =
    rightLens[F, G, K].composeLens(lga)

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