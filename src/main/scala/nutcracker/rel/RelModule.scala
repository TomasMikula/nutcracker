package nutcracker.rel

import nutcracker.Module
import nutcracker.rel.RelLang.{ExecWith, OnPatternMatch, Relate, Supply}
import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}

trait RelModule extends Module {
  def interpreter: Step[Lang, State]
  def freeRelations[F[_[_], _]](implicit i: InjectK[Lang, F]): Relations[FreeK[F, ?]]
}

object RelModule {
  val instance: RelModule = RelModuleImpl
}

private[rel] object RelModuleImpl extends RelModule {
  type Lang[K[_], A] = RelLang[K, A]
  type State[K[_]] = RelDB[K]

  def empty[K[_]]: State[K] = RelDB.empty[K]

  override def freeRelations[F[_[_], _]](implicit i: InjectK[RelLang, F]): Relations[FreeK[F, ?]] =
    RelLang.relationsInstance

  def interpreter: Step[RelLang, RelDB] = new Step[RelLang, RelDB] {
    override def apply[K[_], A](f: RelLang[K, A]): WriterState[Lst[K[Unit]], RelDB[K], A] = f match {
      case r @ Relate(rel, values) => WriterState(db => db.insert(rel, values)(r.ordersWitness, r.orders) match { case (db1, ks) => (ks, db1, ()) })
      case OnPatternMatch(p, a, h) => WriterState(db => db.addOnPatternMatch(p, a)(h) match { case (db1, ks) => (ks, db1, ()) })
      case ExecWith(rel, ass, supp, exec, m, os) => WriterState(db => db.execWith(rel, ass)(supp)(exec)(m, os) match { case (db1, ko) => (Lst.maybe(ko), db1, ()) })
      case Supply(rel, token, value) => WriterState(db => db.supply(rel, token, value) match { case (db1, ks) => (ks, db1, ()) })
    }
  }
}