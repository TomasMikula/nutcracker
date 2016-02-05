package nutcracker.rel

import scala.language.existentials
import scala.language.higherKinds

import nutcracker.rel.RelLang._
import nutcracker.util.Mapped
import nutcracker.util.free.Interpreter
import nutcracker.util.free.Interpreter.{CleanInterpreter, AlwaysClean}

import algebra.Order
import scalaz.{Foldable, Applicative}
import scalaz.std.list._
import shapeless.HList

case class RelDB[K[_]] private (
  private val tables: Map[Rel[_], RelTable[_]],
  private val patternTriggers: Map[Pattern[_], List[_ => K[Unit]]],
  private val relToPatterns: Map[Rel[_], Set[OrientedPattern[_, _]]]
){

  case class Inserter[L <: HList, OS <: HList] private[RelDB] (rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS]) {

    def insert(row: L)(implicit orders: OS, K: Applicative[K]): (RelDB[K], K[Unit]) =  {

      // insert the row to the corresponding table
      table(rel).insert(row) match {

        case None => // row was already present, no additional actions necessary
          (RelDB.this, K.point(()))

        case Some(tbl1) => // successfully inserted

          // search for pattern matches before including the new relation
          val ks: List[K[Unit]] = watchedPatterns(rel) flatMap { collectTriggers(_, row) }

          (replaceTable(rel)(tbl1), Foldable[List].sequence_(ks))
      }
    }
  }

  def into[L <: HList](rel: Rel[L])(implicit m: Mapped[L, Order]): Inserter[L, m.Out] = Inserter(rel)(m)

  def addOnPatternMatch[V <: HList](p: Pattern[V])(h: V => K[Unit])(implicit K: Applicative[K]): (RelDB[K], K[Unit]) = {
    require(p.isCovered, "The domain of the pattern is not fully covered by its relations. These positions are not covered: " + ((0 until p.vertexCount).toSet -- p.vertexSet))

    val matches: List[V] = search(p)

    val k = Foldable[List].traverse_(matches)(h(_))

    (addTrigger(p, h), k)
  }

  private def collectTriggers[V <: HList, L <: HList](op: OrientedPattern[V, L], l: L): List[K[Unit]] = for {
    hit <- search(op, l)
    trig <- triggers(op.pattern)
  } yield trig(hit)

  private def search[V <: HList](p: Pattern[V]): List[V] = {
    // find the size of the table for each relation in the pattern
    val relSizes = p.relations.map(rc => (rc.rel, table0(rc.rel).map(_.size).getOrElse(0)))

    // find the relation with the least rows
    val minRel = relSizes.reduce((r1, r2) => if(r1._2 <= r2._2) r1 else r2)

    // start the search from the minimum relation
    search(p, minRel._1)
  }

  private def search[V <: HList, L <: HList](p: Pattern[V], rel: Rel[L]): List[V] = {
    val op = p.orient(rel)
    table0(rel).map(_.rows).getOrElse(Vector[L]()).toList.flatMap(search(op, _))
  }

  private def search[V <: HList, L <: HList](op: OrientedPattern[V, L], l: L): List[V] =
    op.orientations flatMap { case (r, rs) =>
      search(op.pattern.emptyAssignment.set(r.choose)(l), rs)
    }

  private def search[V <: HList](ass: Assignment[V], rels: List[RelChoice[V, _ <: HList]]): List[V] = rels match {
    case Nil => List(ass.getIfComplete.get) // since we only allow fully covered patterns, the result must be present
    case r::rs => search(ass, r, rs)
  }

  private def search[V <: HList, L <: HList](ass: Assignment[V], rel: RelChoice[V, L], rels: List[RelChoice[V, _ <: HList]]): List[V] = {
    val q = ass.get(rel.choose)
    query(rel.rel)(q) flatMap { (l: L) => search(ass.set(rel.choose)(l), rels) }
  }

  private def table[L <: HList, OS <: HList](rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS], order: OS): RelTable[L] =
    table0(rel).getOrElse(RelTable[L].empty)

  private def table0[L <: HList](rel: Rel[L]): Option[RelTable[L]] =
    tables.get(rel).map(_.asInstanceOf[RelTable[L]])

  // lookup all watched patterns that contain the given rel
  private def watchedPatterns[L <: HList](rel: Rel[L]): List[OrientedPattern[_ <: HList, L]] =
    relToPatterns.get(rel).map(_.toList.asInstanceOf[List[OrientedPattern[_ <: HList, L]]]).getOrElse(Nil)

  private def query[L <: HList](rel: Rel[L])(q: Assignment[L]): List[L] = table0(rel) match {
    case Some(tbl) => tbl.query(q)
    case None => Nil
  }

  private def triggers[V <: HList](p: Pattern[V]): List[V => K[Unit]] =
    patternTriggers.getOrElse(p, Nil).asInstanceOf[List[V => K[Unit]]]

  private def replaceTable[L <: HList](rel: Rel[L])(tbl: RelTable[L]): RelDB[K] =
    copy(tables = tables + ((rel, tbl)))

  private def addTrigger[V <: HList](p: Pattern[V], h: V => K[Unit]): RelDB[K] =
    copy(patternTriggers = patternTriggers + ((p, h :: triggers(p))))

}

object RelDB {

  implicit def interpreter: Interpreter[RelLang, RelDB, AlwaysClean] = new CleanInterpreter[RelLang, RelDB] {

    def step0[K[_] : Applicative, A](f: RelLang[K, A])(db: RelDB[K]): (RelDB[K], K[A]) = f match {
      case r @ Relate(rel, values) => db.into(rel)(r.ordersWitness).insert(values)(r.orders, Applicative[K])
      case OnPatternMatch(p, h) => db.addOnPatternMatch(p)(h)
    }

  }
}