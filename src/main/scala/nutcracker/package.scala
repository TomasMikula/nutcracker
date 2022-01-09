package object nutcracker {

  type ObserveVal[M[_], Val0[_]] = Observe[M] { type Val[A] = Val0[A] }

  opaque type UpdateResult[D, Δ] = IUpdateResult[[i] =>> D, [i, j] =>> Δ, ?, ?]

  object UpdateResult {
    def unchanged[D, Δ]: UpdateResult[D, Δ] =
      IUpdateResult.unchanged[[i] =>> D, [i, j] =>> Δ, Any]

    def updated[D, Δ](newValue: D, δ: Δ): UpdateResult[D, Δ] =
      IUpdateResult.updated[[i] =>> D, [i, j] =>> Δ, Any, Any](newValue, δ)

    def apply[D, Δ](res: Option[(D, Δ)]): UpdateResult[D, Δ] = res match {
      case Some((d, δ)) => updated(d, δ)
      case None         => unchanged[D, Δ]
    }

    def apply[D, Δ](iRes: IUpdateResult[[i] =>> D, [i, j] =>> Δ, ?, ?]): UpdateResult[D, Δ] =
      iRes

    extension [D, Δ](r: UpdateResult[D, Δ]) {
      def at[I, J]: IUpdateResult[[i] =>> D, [i, j] =>> Δ, I, J] =
        r.asInstanceOf[IUpdateResult[[i] =>> D, [i, j] =>> Δ, I, J]]

      def newValueOr(default: D): D =
        r match {
          case Updated(d, _) => d
          case Unchanged()   => default
        }

      def map[E, Δ2](f: D => E, g: Δ => Δ2): UpdateResult[E, Δ2] =
        r match {
          case Updated(d, δ) => updated(f(d), g(δ))
          case Unchanged()   => unchanged
        }

      def mapDomain[E](f: D => E): UpdateResult[E, Δ] =
        r match {
          case Updated(d, δ) => updated(f(d), δ)
          case Unchanged()   => unchanged
        }
    }
  }
}
