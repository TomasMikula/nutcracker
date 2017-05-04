package nutcracker.toolkit

import nutcracker.rel.Relations
import nutcracker.util.{FreeK, InjectK, Lst, Step, WriterState}
import scalaz.Monad

trait RelModule extends Module {
  def interpreter: Step[Lang, StateK]
  def freeRelations[F[_[_], _]](implicit i: InjectK[Lang, F]): Relations[FreeK[F, ?]]
}

object RelModule {
  val instance: PersistentRelModule = RelModuleImpl
}

trait PersistentRelModule extends RelModule with PersistentStateModule { self =>
  override def stashable: StashRelModule { type Lang[K[_], A] = self.Lang[K, A] }
}

object PersistentRelModule {
  type Aux[Lang0[_[_], _], State0[_[_]]] = PersistentRelModule {
    type Lang[K[_], A] = Lang0[K, A]
    type StateK[K[_]] = State0[K]
  }
}

trait StashRelModule extends RelModule with StashModule

private[toolkit] object RelModuleImpl extends PersistentRelModule {
  type Lang[K[_], A] = RelLang[K, A]
  type StateK[K[_]] = RelDB[K]

  def emptyK[K[_]]: StateK[K] = RelDB.empty[K]

  override def freeRelations[F[_[_], _]](implicit i: InjectK[RelLang, F]): Relations[FreeK[F, ?]] =
    RelLang.relationsInstance

  def interpreter: Step[RelLang, RelDB] = new Step[RelLang, RelDB] {
    import RelLang._
    override def apply[K[_]: Monad, A](f: RelLang[K, A]): WriterState[Lst[K[Unit]], RelDB[K], A] = f match {
      case r @ Relate(rel, values) => WriterState(db => db.insert(rel, values)(r.ordersWitness, r.orders) match { case (db1, ks) => (ks, db1, ()) })
      case OnPatternMatch(p, a, h) => WriterState(db => db.addOnPatternMatch(p, a)(h) match { case (db1, ks) => (ks, db1, ()) })
      case ExecWith(rel, ass, supp, exec, m, os) => WriterState(db => db.execWith(rel, ass)(supp)(exec)(m, os) match { case (db1, ko) => (Lst.maybe(ko), db1, ()) })
      case Supply(rel, token, value) => WriterState(db => db.supply(rel, token, value) match { case (db1, ks) => (ks, db1, ()) })
    }
  }

  def stashable = new RelListModule[Lang, StateK](this)
}

private[toolkit] class RelListModule[Lang[_[_], _], State0[_[_]]](base: PersistentRelModule.Aux[Lang, State0]) extends ListModule[Lang, State0](base) with StashRelModule {
  def freeRelations[F[_[_], _]](implicit i: InjectK[Lang, F]) = base.freeRelations[F]
  def interpreter: Step[Lang, StateK] = base.interpreter.inHead
}