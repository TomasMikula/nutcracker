package nutcracker.lib

import nutcracker.{Dom, Propagation, SyncDom, Trigger}
import nutcracker.rel.Recipe
import nutcracker.rel.Rel.Rel3
import nutcracker.util.{Choose, ∃}
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

  def recipe[A, B, Ref[_], K[_]](implicit P: Propagation[K, Ref], K: Monad[K], da: SyncDom[A], db: SyncDom[B]): Recipe[L[A, B, Ref], C[A, B, Ref], K] =
    new Recipe[L[A, B, Ref], C[A, B, Ref], K](_0 :: _1 :: Choose[L[A, B, Ref]]) {

      override def create(ingredients: C[A, B, Ref]): K[L[A, B, Ref]] = {
        implicit val dom = Dom.tuple2[A, B]
        val ra :: rb :: HNil = ingredients

        P.observe(ra).byM[Ref[(A, B)]](a =>
          P.observe(rb).byM[Ref[(A, B)]](b =>
            P.newCell((a, b)) map (rr => (Trigger.sleep(Trigger.continually((b, δb) => P.update(rr).by(\&/.That(db.toPatch(b, δb))))), rr))
          ) map                   (rr => (Trigger.sleep(Trigger.continually((a, δa) => P.update(rr).by(\&/.This(da.toPatch(a, δa))))), rr))
        ) map (rr => ra :: rb :: rr :: HNil)
      }

    }

}
