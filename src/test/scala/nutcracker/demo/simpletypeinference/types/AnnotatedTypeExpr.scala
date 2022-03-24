package nutcracker.demo.simpletypeinference.types

import scalaz.Monad
import scalaz.syntax.monad._

import nutcracker.demo.simpletypeinference.kinds._

case class AnnotatedTypeExpr[A[_, _], K, L](
  annotation: A[K, L],
  value: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], K, L, ?],
) {
  def applyTo[J, M[_]: Monad](
    a: ArgIntro[A[○, *], J, K],
  )(
    ann: [k, l] => generic.TypeExpr[A, k, l, ?] => M[A[k, l]],
  ): M[generic.TypeExpr[A, J, L, ?]] =
    a.inKind.properKind match {
      case Left(j_eq_○) =>
        j_eq_○.substituteContra[[j] =>> M[generic.TypeExpr[A, j, L, ?]]](
          applyTo0(j_eq_○.substituteCo[ArgIntro[A[○, *], *, K]](a))(ann)
        )
      case Right(j) =>
        applyTo1(a)(ann)(using j, summon[Monad[M]])
    }

  private def applyTo0[M[_]: Monad](
    args: ArgIntro[A[○, *], ○, K],
  )(
    ann: [k, l] => generic.TypeExpr[A, k, l, ?] => M[A[k, l]],
  ): M[generic.TypeExpr[A, ○, L, ?]] = {
    import generic.{TypeExpr => gt}

    this.value match {
      case gt.AppFst(op, arg1) =>
        import op.in2Kind
        val arg2 = ArgIntro.unwrap(args)
        Monad[M].pure(gt.BiApp(op.cast, arg1.annotation, arg2))

      case gt.AppCompose(op, a, g) =>
        for {
          b <- g.applyTo0(args)(ann)
          bc <- ann(b)
        } yield gt.BiApp(op.cast, a.annotation, bc)

      case gt.PFix(pre, expr) =>
        val a: ArgIntro[A[○, *], ●, K × ●] = ArgIntro.introFst(args)
        pre.applyTo(a) match {
          case Routing.ApplyRes(r, a1) =>
            for {
              expr1 <- expr.applyTo(a1)(ann)
              expr2 <- ann(expr1)
            } yield gt.Fix(r, expr2)
        }

      case other =>
        throw new NotImplementedError(s"Supplying $args into $other")
    }
  }

  private def applyTo1[J: ProperKind, M[_]: Monad](
    args: ArgIntro[A[○, *], J, K],
  )(
    ann: [k, l] => generic.TypeExpr[A, k, l, ?] => M[A[k, l]],
  ): M[generic.TypeExpr[A, J, L, ?]] = {
    import generic.{TypeExpr => gt}

    this.value match {
      case gt.AppCompose(op, a, g) =>
        for {
          h <- g.applyTo1(args)(ann)
          hc <- ann(h)
        } yield
          gt.AppCompose(op.cast, a.annotation, hc)

      case gt.Pair() =>
        args match {
          case ArgIntro.IntroFst(a) =>
            gt.pair1(ArgIntro.unwrap(a)).pure[M]
          case other =>
            throw new NotImplementedError(s"$other")
        }

      case other =>
        throw new NotImplementedError(s"$other")
    }
  }
}

object AnnotatedTypeExpr {
  def annotations[A[_, _], K, L, I](e: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], K, L, I]): generic.TypeExpr[A, K, L, I] =
    e.translate[A]([k, l] => (ae: AnnotatedTypeExpr[A, k, l]) => ae.annotation)

  def annotate[A[_, _], K, L](
    f: TypeExpr[K, L],
    ann: [k, l] => generic.TypeExpr[A, k, l, ?] => A[k, l],
  ): AnnotatedTypeExpr[A, K, L] =
    annotateM[A, K, L, scalaz.Id.Id](f, ann)(using scalaz.Id.id)

  def annotateM[A[_, _], K, L, M[_]: Monad](
    f: TypeExpr[K, L],
    ann: [k, l] => generic.TypeExpr[A, k, l, ?] => M[A[k, l]],
  ): M[AnnotatedTypeExpr[A, K, L]] =
    f.foldM[AnnotatedTypeExpr[A, *, *], M](
      [k, l] => (tf: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], k, l, ?]) => {
        val ta: generic.TypeExpr[A, k, l, ?] = tf.translate[A](getAnnotation[A])
        ann(ta).map(AnnotatedTypeExpr(_, tf))
      }
    )

  private def getAnnotation[A[_, _]]: [x, y] => AnnotatedTypeExpr[A, x, y] => A[x, y] =
    [x, y] => (ate: AnnotatedTypeExpr[A, x, y]) => ate.annotation
}
