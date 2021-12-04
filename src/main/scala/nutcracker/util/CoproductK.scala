package nutcracker.util

import scalaz.Coproduct

object CoproductK {

  sealed trait Builder {
    type Out[_[_], _]

    def or[G[_[_], _]]: Builder.Aux[Out, G] =
      Builder.coproduct[Out, G]
  }

  object Builder {
    type Aux[F[_[_], _], G[_[_], _]] =
      Builder {
        type Out[K[_], A] = Coproduct[F[K, *], G[K, *], A]
      }

    def coproduct[F[_[_], _], G[_[_], _]]: Builder.Aux[F, G] =
      new Builder {
        type Out[K[_], A] = Coproduct[F[K, *], G[K, *], A]
      }
  }

  object zero {
    def or[F[_[_], _]]: Builder { type Out[K[_], A] = F[K, A] } =
      new Builder { type Out[K[_], A] = F[K, A] }
  }
}