package nutcracker.util.free

import scalaz.~>

sealed trait CoproductBuilder {
  type Out[_]
}

sealed trait :++:[F[_], G[_]] extends CoproductBuilder {
  type Out[A] = Either[F[A], G[A]]
}

sealed trait :+:[F[_], B <: CoproductBuilder] extends CoproductBuilder {
  type Out[A] = Either[F[A], B#Out[A]]
}

object Coproduct {
  def injectLeft[F[_], G[_], H[_]](f: F ~> G): (F :++: H)#Out ~> (G :++: H)#Out =
    λ[(F :++: H)#Out ~> (G :++: H)#Out](_ match {
      case Left(fa) => Left(f(fa))
      case Right(ha) => Right(ha)
    })

  def injectRight[F[_], G[_], H[_]](f: G ~> H): (F :++: G)#Out ~> (F :++: H)#Out =
    λ[(F :++: G)#Out ~> (F :++: H)#Out](_ match {
      case Left(fa) => Left(fa)
      case Right(ga) => Right(f(ga))
    })
}