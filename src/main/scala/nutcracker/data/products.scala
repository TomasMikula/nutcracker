package nutcracker.data

import nutcracker.{Dom, OnDemandPropagation, Recipe, Relations, Subscription, SyncDom}
import nutcracker.Rel.Rel3
import nutcracker.util.{Choose, ContU, HOrderK, Id, IndexedContT}
import nutcracker.util.HList.{::, HNil}
import nutcracker.util.Nat.{_0, _1}
import scalaz.{Monad, Order, \&/}
import scalaz.syntax.monad._

/** Product of two cells.
  *
  * The flow is unidirectional, from factors to the product, but not the other way around.
  *
  * Queries have to specify a superset of either both factors, or the result.
  * A query that specifies just one factor doesn't uniquely determine the types
  * of the other columns, and therefore is not considered type-safe.
  */
case class Tupled2[A, B, Ref[_]]() extends Rel3[Ref[A], Ref[B], Ref[(A, B)]] {
  type Projection = Ref[A] :: Ref[B] :: Ref[(A, B)] :: HNil
}

object Tupled2 {
  private type L[A, B, Ref[_]] = Ref[A] :: Ref[B] :: Ref[(A, B)] :: HNil
  private type C[A, B, Ref[_]] = Ref[A] :: Ref[B]                :: HNil

  def establish[A, B, Var[_], Val[_], K[_]](
    ra: Val[A],
    rb: Val[B],
  )(implicit
    P: OnDemandPropagation.Aux1[K, Var, Val],
    R: Relations[K],
    O: HOrderK[Val],
    da: SyncDom[A],
    db: SyncDom[B],
  ): ContU[K, Val[(A, B)]] = {
    implicit def orderVal[T]: Order[Val[T]] = O.specialize[T]

    R
      .establish(Tupled2[A, B, Val]())
      .matching2(ra, rb)
      .by(recipe)
      .map(_.tail.tail.head)
  }

  def recipe[A, B, Var[_], Val[_], K[_]](implicit
    P: OnDemandPropagation.Aux1[K, Var, Val],
    da: SyncDom[A],
    db: SyncDom[B],
  ): Recipe[L[A, B, Val], C[A, B, Val], K] =
    new Recipe[L[A, B, Val], C[A, B, Val], K](_0 :: _1 :: Choose[L[A, B, Val]]) {
      import P.{Val => _, _}

      override def create(ingredients: C[A, B, Val]): K[L[A, B, Val]] = {
        implicit val dom: Dom.Aux[(A, B), da.Update \&/ db.Update, da.Delta \&/ db.Delta] = Dom.tuple2[A, B]
        val ra :: rb :: HNil = ingredients

        newAutoCellC(IndexedContT((f: ((A, B)) => K[ExclRef[(A, B)]]) =>
          observe(ra).byC[(Subscription[K], ExclRef[(A, B)])](a =>
            observe(rb).byM[ExclRef[(A, B)]](b =>
              f((a, b)) map (rr => (P.sleep(P.continually((b, δb) => P.exclUpdate(rr).by(\&/.That(db.toPatch(b, δb))))), rr))
            ).map({
              case (sub1, rr)   => (P.sleep(P.continually((a, δa) => P.exclUpdate(rr).by(\&/.This(da.toPatch(a, δa))))), (sub1, rr))
            })
          ).run(Id { case (sub2, (sub1, rr)) => P.addFinalizer(rr, sub1 and sub2).void })
        )).map(rr => ra :: rb :: rr :: HNil)
      }

    }

}
