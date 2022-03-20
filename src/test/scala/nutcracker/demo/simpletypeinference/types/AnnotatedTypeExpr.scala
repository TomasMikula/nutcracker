package nutcracker.demo.simpletypeinference.types

import scalaz.Monad
import scalaz.syntax.monad._

case class AnnotatedTypeExpr[A[_, _], K, L](
  annotation: A[K, L],
  value: generic.TypeExpr[AnnotatedTypeExpr[A, *, *], K, L, ?],
)

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
