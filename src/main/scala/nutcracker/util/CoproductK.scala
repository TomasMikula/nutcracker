package nutcracker.util

import scalaz.Coproduct

object CoproductK {

  sealed trait Builder {
    type Out[_[_], _]
  }

  sealed trait :++:[F[_[_], _], G[_[_], _]] extends Builder {
    type Out[K[_], A] = Coproduct[F[K, *], G[K, *], A]
  }

  sealed trait :+:[F[_[_], _], B <: Builder] extends Builder {
    type Out[K[_], A] = Coproduct[F[K, *], B#Out[K, *], A]
  }
}