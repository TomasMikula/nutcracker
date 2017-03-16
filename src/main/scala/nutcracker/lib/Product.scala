package nutcracker.lib

import nutcracker.{Dom, Propagation, RDom, Trigger}
import nutcracker.rel.Recipe
import nutcracker.rel.Rel.Rel3
import nutcracker.util.Choose
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
class Product2[A, B, Ref[_]] extends Rel3[Ref[A], Ref[B], Ref[(A, B)]] {
  private val choose: Choose[Row, Ref[A] :: Ref[B] :: HNil] = _0 :: _1 :: Choose[Row]

  def recipe[K[_]](implicit P: Propagation[K, Ref], K: Monad[K], da: RDom[A], db: RDom[B]): Recipe[Row, Ref[A] :: Ref[B] :: HNil, K] =
    new Recipe[Row, Ref[A] :: Ref[B] :: HNil, K](choose) {

      override def create(ingredients: Ref[A] :: Ref[B] :: HNil): K[Row] = {
        implicit val dom = Dom.tuple2[A, B]
        val ra :: rb :: HNil = ingredients

        P.observe(ra).byM[Ref[(A, B)]](a =>
          P.observe(rb).byM[Ref[(A, B)]](b =>
            P.newCell((a, b)) map (rr => (Trigger.sleep(Trigger.continually((_, δb) => P.update(rr).by(\&/.That(db.recur(δb))))), rr))
          ) map                   (rr => (Trigger.sleep(Trigger.continually((_, δa) => P.update(rr).by(\&/.This(da.recur(δa))))), rr))
        ) map (rr => ra :: rb :: rr :: HNil)
      }

    }

}