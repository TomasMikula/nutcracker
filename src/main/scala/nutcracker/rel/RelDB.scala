package nutcracker.rel

import scala.language.existentials
import scala.language.higherKinds
import nutcracker.util.{KMapB, Lst, Mapped, TransformedIndex}
import algebra.Order
import shapeless.HList
import RelDB._

private[rel] case class RelDB[K[_]] private (
  private val tables: KMapB[Rel, λ[`L <: HList` => RelTable[K, L]], HList],
  private val patternTriggers: Map[PartiallyAssignedPattern[_], List[_ => K[Unit]]],
  private val relToPatterns: TransformedIndex[Rel[_ <: HList], PartiallyAssignedPattern[_ <: HList], PartiallyAssignedOrientedPattern[_ <: HList, _ <: HList]]
) {

  def insert[L <: HList, OS <: HList](rel: Rel[L], row: L)(implicit m: Mapped.Aux[L, Order, OS], orders: OS): (RelDB[K], Lst[K[Unit]]) =
    table(rel).insert(row) match {

      case None => // row was already present, no additional actions necessary
        (RelDB.this, Lst.empty)

      case Some(tbl1) => // successfully inserted

        // search for pattern matches before including the new relation
        val ks: Lst[K[Unit]] = collectTriggers(rel, row)

        (replaceTable(rel)(tbl1), ks)
    }

  def addOnPatternMatch[V <: HList](p: Pattern[V], ass: Assignment[V])(h: V => K[Unit]): (RelDB[K], Lst[K[Unit]]) = {
    require(p.isCovered, "The domain of the pattern is not fully covered by its relations. These positions are not covered: " + ((0 until p.vertexCount).toSet -- p.vertexSet))

    val matches: List[V] = search(p, ass)

    val ks = matches.foldLeft(Lst.empty[K[Unit]])((l, v) => h(v) :: l)

    (addTrigger(PartiallyAssignedPattern(p, ass), h), ks)
  }

  def execWith[L <: HList, OS <: HList](rel: Rel[L], ass: Assignment[L])(supply: Token[L] => K[Unit])(exec: L => K[Unit])(implicit m: Mapped.Aux[L, Order, OS], orders: OS): (RelDB[K], Option[K[Unit]]) = {
    val (tbl, k) = table(rel).execWith(ass)(supply)(exec)
    tbl match {
      case Some(tbl) => (replaceTable(rel)(tbl), k)
      case None      => (this                  , k)
    }
  }

  def supply[L <: HList](rel: Rel[L], t: Token[L], row: L): (RelDB[K], Lst[K[Unit]]) = {
    val ks1 = collectTriggers(rel, row) // trigger patterns matching the new row
    val (tbl, ks2) = table0(rel).get.supply(t, row)
    (replaceTable(rel)(tbl), ks1 ::: ks2)
  }

  private def collectTriggers[L <: HList](rel: Rel[L], row: L): Lst[K[Unit]] =
    watchedPatterns(rel).foldLeft(Lst.empty[K[Unit]])((l, p) => collectTriggers(p, row) rev_::: l)

  private def collectTriggers[V <: HList, L <: HList](paop: PartiallyAssignedOrientedPattern[V, L], row: L): List[K[Unit]] = for {
    hit <- search(paop, row)
    trig <- triggers(paop.unorient)
  } yield trig(hit)

  private def search[V <: HList](p: Pattern[V], initAss: Assignment[V]): List[V] = {
    // find the size of the table for each relation in the pattern
    val relSizes = p.relations.map(rc => (rc.rel, table0(rc.rel).map(_.size).getOrElse(0)))

    // find the relation with the least rows
    val minRel = relSizes.reduce((r1, r2) => if(r1._2 <= r2._2) r1 else r2)

    // start the search from the minimum relation
    search(p, initAss, minRel._1)
  }

  private def search[V <: HList, L <: HList](p: Pattern[V], initAss: Assignment[V], rel: Rel[L]): List[V] = {
    table0(rel) match {
      case None => Nil
      case Some(tbl) =>
        p.orient(rel).orientations flatMap { case (r, rs) =>
          tbl.query(initAss.get(r.choose)) flatMap { l =>
            search(initAss.set(r.choose)(l), rs)
          }
        }
    }
  }

  private def search[V <: HList, L <: HList](paop: PartiallyAssignedOrientedPattern[V, L], l: L): List[V] =
    search(paop.pattern, paop.assignment, l)

  private def search[V <: HList, L <: HList](op: OrientedPattern[V, L], initAss: Assignment[V], l: L): List[V] =
    op.orientations flatMap { case (r, rs) =>
      initAss.extend(r.choose)(l) match {
        case None => Nil
        case Some(ass) => search(ass, rs)
      }
    }

  private def search[V <: HList](ass: Assignment[V], rels: List[RelChoice[V, _ <: HList]]): List[V] = rels match {
    case Nil => List(ass.getIfComplete.get) // since we only allow fully covered patterns, the result must be present
    case r::rs => search(ass, r, rs)
  }

  private def search[V <: HList, L <: HList](ass: Assignment[V], rel: RelChoice[V, L], rels: List[RelChoice[V, _ <: HList]]): List[V] = {
    val q = ass.get(rel.choose)
    query(rel.rel)(q) flatMap { (l: L) => search(ass.set(rel.choose)(l), rels) }
  }

  private def table[L <: HList, OS <: HList](rel: Rel[L])(implicit m: Mapped.Aux[L, Order, OS], order: OS): RelTable[K, L] =
    table0(rel).getOrElse(RelTable[K, L].empty)

  private def table0[L <: HList](rel: Rel[L]): Option[RelTable[K, L]] =
    tables.get(rel)

  // lookup all watched patterns that contain the given rel
  private def watchedPatterns[L <: HList](rel: Rel[L]): List[PartiallyAssignedOrientedPattern[_ <: HList, L]] =
    relToPatterns.get(rel).toList.asInstanceOf[List[PartiallyAssignedOrientedPattern[_ <: HList, L]]]

  private def query[L <: HList](rel: Rel[L])(q: Assignment[L]): List[L] = table0(rel) match {
    case Some(tbl) => tbl.query(q)
    case None => Nil
  }

  private def triggers[V <: HList](p: PartiallyAssignedPattern[V]): List[V => K[Unit]] =
    patternTriggers.getOrElse(p, Nil).asInstanceOf[List[V => K[Unit]]]

  private def replaceTable[L <: HList](rel: Rel[L])(tbl: RelTable[K, L]): RelDB[K] =
    copy(tables = tables.put(rel)(tbl))

  private def addTrigger[V <: HList](p: PartiallyAssignedPattern[V], h: V => K[Unit]): RelDB[K] =
    copy(
      patternTriggers = patternTriggers + ((p, h :: triggers(p))),
      relToPatterns = relToPatterns.add(p)
    )
}

private[rel] object RelDB {

  private[rel] case class PartiallyAssignedPattern[V <: HList](pattern: Pattern[V], assignment: Assignment[V]) {
    def orient[L <: HList](rel: Rel[L]): PartiallyAssignedOrientedPattern[V, L] =
      PartiallyAssignedOrientedPattern(pattern.orient(rel), assignment)
  }
  private[rel] case class PartiallyAssignedOrientedPattern[V <: HList, L <: HList](pattern: OrientedPattern[V, L], assignment: Assignment[V]) {
    def unorient: PartiallyAssignedPattern[V] = PartiallyAssignedPattern(pattern.pattern, assignment)
  }

  def empty[K[_]]: RelDB[K] = RelDB(
    KMapB[Rel, λ[`L <: HList` => RelTable[K, L]], HList](),
    Map.empty,
    TransformedIndex.empty(_.pattern.relations.map(_.rel), (pap, rel) => pap.orient(rel))
  )
}