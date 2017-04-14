package nutcracker.lib

import nutcracker.{Dom, Propagation, Subscription, SyncDom}
import nutcracker.rel.{Recipe, Relations}
import nutcracker.rel.Rel.Rel3
import nutcracker.util.{Choose, ContU, HOrderK, ∃}
import scalaz.{Monad, \&/}
import scalaz.syntax.monad._
import shapeless.{::, HNil}
import shapeless.nat._

/** Product of two cells.
  *
  * The flow is unidirectional, from factors to the product, but not the other way around.
  *
  * Queries have to specify a superset of either both factors, or the result.
  * A query that specifies just one factor doesn't uniquely determine the types
  * of the other columns, and therefore is not considered type-safe.
  */
case class Tupled2[A, B, Ref[_]]() extends Rel3[Ref[A], Ref[B], Ref[(A, B)]] {
  type Projection = ∃[Ref] :: ∃[Ref] :: ∃[Ref] :: HNil
}

object Tupled2 {
  private type L[A, B, Ref[_]] = Ref[A] :: Ref[B] :: Ref[(A, B)] :: HNil
  private type C[A, B, Ref[_]] = Ref[A] :: Ref[B]                :: HNil

  def establish[A, B, Var[_], Val[_], K[_]](ra: Var[A], rb: Var[B])(implicit P: Propagation[K, Var, Val], R: Relations[K], O: HOrderK[Var], K: Monad[K], da: SyncDom[A], db: SyncDom[B]): ContU[K, Var[(A, B)]] = {
    R.establish(Tupled2[A, B, Var]).matching2(ra, rb).by(recipe).map(_.tail.tail.head)
  }

  def recipe[A, B, Var[_], Val[_], K[_]](implicit P: Propagation[K, Var, Val], K: Monad[K], da: SyncDom[A], db: SyncDom[B]): Recipe[L[A, B, Var], C[A, B, Var], K] =
    new Recipe[L[A, B, Var], C[A, B, Var], K](_0 :: _1 :: Choose[L[A, B, Var]]) {
      import P._

      override def create(ingredients: C[A, B, Var]): ContU[K, (L[A, B, Var], Subscription[K])] = {
        implicit val dom = Dom.tuple2[A, B]
        val ra :: rb :: HNil = ingredients

        observe(ra).byC[(Subscription[K], Var[(A, B)])] { a =>
          observe(rb).byM[Var[(A, B)]](b =>
            newCell((a, b)) map (rr => (P.sleep(P.continually((b, δb) => P.update(rr).by(\&/.That(db.toPatch(b, δb))))), rr))
          ).map({     case (sub1, rr) => (P.sleep(P.continually((a, δa) => P.update(rr).by(\&/.This(da.toPatch(a, δa))))), (sub1, rr)) })
        } map { case (sub2, (sub1, rr)) => (ra :: rb :: rr :: HNil, sub1 and sub2) }
      }

    }

}
