package nutcracker.util

import monocle.Lens

import scala.language.higherKinds
import scalaz.Monoid

final case class ProductK[F[_], G[_], A](_1: F[A], _2: G[A]) {
  def update_1(f: F[A]) = ProductK[F, G, A](f, _2)
  def update_2(g: G[A]) = ProductK[F, G, A](_1, g)
  def :*:[E[_]](e: E[A]): ProductK[E, ProductK[F, G, ?], A] = ProductK[E, ProductK[F, G, ?], A](e, this)
}

object ProductK {
  implicit class ProductKOps[G[_], A](g: G[A]) {
    def :*:[F[_]](f: F[A]): ProductK[F, G, A] = ProductK(f, g)
  }

  implicit def leftLens[F[_], G[_], A]: Lens[ProductK[F, G, A], F[A]] =
    Lens[ProductK[F, G, A], F[A]](_._1)(fa => pa => pa.update_1(fa))

  implicit def leftLensZ[F[_], G[_], A]: scalaz.Lens[ProductK[F, G, A], F[A]] =
    scalaz.Lens[ProductK[F, G, A], F[A]](pa => scalaz.Store(fa => pa.update_1(fa), pa._1))

  implicit def leftLensZK[F[_], G[_]]: ValA[λ[A => scalaz.Lens[ProductK[F, G, A], F[A]]]] =
    new ValA[λ[A => scalaz.Lens[ProductK[F, G, A], F[A]]]] {
      def compute[A]: scalaz.Lens[ProductK[F, G, A], F[A]] =
        scalaz.Lens[ProductK[F, G, A], F[A]] (pa => scalaz.Store (fa => pa.update_1 (fa), pa._1) )
    }

  implicit def rightLens[F[_], G[_], A]: Lens[ProductK[F, G, A], G[A]] =
    Lens[ProductK[F, G, A], G[A]](_._2)(ga => pa => pa.update_2(ga))

  implicit def rightLensZ[F[_], G[_], A]: scalaz.Lens[ProductK[F, G, A], G[A]] =
    scalaz.Lens[ProductK[F, G, A], G[A]](pa => scalaz.Store(ga => pa.update_2(ga), pa._2))

  implicit def rightLensZK[F[_], G[_]]: ValA[λ[A => scalaz.Lens[ProductK[F, G, A], G[A]]]] =
    new ValA[λ[A => scalaz.Lens[ProductK[F, G, A], G[A]]]] {
      def compute[A]: scalaz.Lens[ProductK[F, G, A], G[A]] =
        scalaz.Lens[ProductK[F, G, A], G[A]](pa => scalaz.Store(ga => pa.update_2(ga), pa._2))
    }

  implicit def leftComposedLens[F[_], G[_], K, A](implicit lfa: Lens[F[K], A]): Lens[ProductK[F, G, K], A] =
    leftLens[F, G, K].composeLens(lfa)

  implicit def rightComposedLens[F[_], G[_], K, A](implicit lga: Lens[G[K], A]): Lens[ProductK[F, G, K], A] =
    rightLens[F, G, K].composeLens(lga)

  implicit def monoidK[F[_], G[_]](implicit F: MonoidK[F], G: MonoidK[G]): MonoidK[ProductK[F, G, ?]] = new MonoidK[ProductK[F, G, ?]] {
    def zero[A]: ProductK[F, G, A] = ProductK(F.zero, G.zero)

    def append[A](p1: ProductK[F, G, A], p2: ProductK[F, G, A]): ProductK[F, G, A] =
      ProductK(F.append(p1._1, p2._1), G.append(p1._2, p2._2))
  }

  implicit def monoid[F[_], G[_], A](implicit F: Monoid[F[A]], G: Monoid[G[A]]): Monoid[ProductK[F, G, A]] = new Monoid[ProductK[F, G, A]] {
    def zero: ProductK[F, G, A] = ProductK(F.zero, G.zero)

    def append(p1: ProductK[F, G, A], p2: => ProductK[F, G, A]): ProductK[F, G, A] =
      ProductK(F.append(p1._1, p2._1), G.append(p1._2, p2._2))
  }
}
